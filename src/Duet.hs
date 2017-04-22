{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A clear-to-read, well-documented, implementation of a Haskell 98
-- type checker adapted from Typing Haskell In Haskell, by Mark
-- P. Jones.

module Duet
  (
  -- * Type checker
  -- $type-checker
    typeCheckModule
  , InferException(..)
  -- * Setting up
  , addClass
  , addInstance
  , SpecialTypes(..)
  , ClassEnvironment(..)
  , ReadException(..)
  -- * Printers
  -- , printTypeSignature
  -- * Types syntax tree
  , Type(..)
  , Kind(..)
  , Scheme(..)
  , TypeSignature(..)
  , TypeVariable(..)
  , Qualified(..)
  , Class(..)
  , Predicate(..)
  , TypeConstructor(..)
  -- * Values syntax tree
  , ImplicitlyTypedBinding(..)
  , ExplicitlyTypedBinding(..)
  , Expression(..)
  , Literal(..)
  , Pattern(..)
  , BindGroup(..)
  , Alternative(..)
  -- * Identifiers
  , Identifier(..)
  ) where

import           Control.Monad.Catch
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Duet.Types

--------------------------------------------------------------------------------
-- Type inference
--

-- $type-checker
--
-- The type checker takes a module and produces a list of type
-- signatures. It checks that all types unify, and infers the types of
-- unannotated expressions. It resolves type-class instances.

-- | Type check the given module and produce a list of type
-- signatures.
--
-- >>> fmap (map printTypeSignature) (typeCheckModule mempty [] [BindGroup [] [[ImplicitlyTypedBinding (Identifier "id") [Alternative [VariablePattern (Identifier "x")] (VariableExpression (Identifier "x"))]]]])
-- ["id :: forall a0. a0 -> a0"]
--
-- Throws 'InferException' in case of a type error.
typeCheckModule
  :: MonadThrow m
  => ClassEnvironment -- ^ Set of defined type-classes.
  -> [(TypeSignature Identifier)] -- ^ Pre-defined type signatures e.g. for built-ins or FFI.
  -> SpecialTypes -- ^ Special types that Haskell uses for pattern matching and literals.
  -> [BindGroup l] -- ^ Bindings in the module.
  -> m [BindGroup (TypeSignature l)] -- ^ Inferred types for all identifiers.
typeCheckModule ce as specialTypes bgs =
  evalStateT
    (runInferT $ do
       (ps, _, bgs') <- inferSequenceTypes inferBindGroupTypes ce as bgs
       s <- InferT (gets inferStateSubstitutions)
       let rs = reduce ce (map (substitutePredicate s) ps)
       s' <- defaultSubst ce [] rs
       return (map (fmap (substituteTypeSignature (s' @@ s))) bgs'))
    (InferState nullSubst 0 specialTypes) {-[]-}

--------------------------------------------------------------------------------
-- Substitution

infixr 4 @@
(@@) :: [Substitution] -> [Substitution] -> [Substitution]
s1 @@ s2 = [Substitution u (substituteType s1 t) | (Substitution u t) <- s2] ++ s1

nullSubst :: [Substitution]
nullSubst = []

substituteQualified :: [Substitution] -> Qualified Type -> Qualified Type
substituteQualified substitutions (Qualified predicates t) =
  Qualified
    (map (substitutePredicate substitutions) predicates)
    (substituteType substitutions t)

substituteTypeSignature :: [Substitution] -> (TypeSignature l) -> (TypeSignature l)
substituteTypeSignature substitutions (TypeSignature l scheme) =
    TypeSignature l (substituteInScheme substitutions scheme)
  where substituteInScheme substitutions (Forall kinds qualified) =
          Forall kinds (substituteQualified substitutions qualified)

substitutePredicate :: [Substitution] -> Predicate -> Predicate
substitutePredicate substitutions (IsIn identifier types) =
    IsIn identifier (map (substituteType substitutions) types)

substituteType :: [Substitution] -> Type -> Type
substituteType substitutions (VariableType typeVariable) =
    case find ((== typeVariable) . substitutionTypeVariable) substitutions of
      Just substitution -> substitutionType substitution
      Nothing -> VariableType typeVariable
substituteType substitutions (ApplicationType type1 type2) =
    ApplicationType
      (substituteType substitutions type1)
      (substituteType substitutions type2)
substituteType _ typ = typ

--------------------------------------------------------------------------------
-- Type inference

unify :: MonadThrow m => Type -> Type -> InferT m ()
unify t1 t2 = do
  s <- InferT (gets inferStateSubstitutions)
  u <- unifyTypes (substituteType s t1) (substituteType s t2)
  InferT
    (modify
       (\s -> s {inferStateSubstitutions = u @@ inferStateSubstitutions s}))

newVariableType :: Monad m => Kind -> InferT m Type
newVariableType k =
  InferT
    (do inferState <- get
        put inferState {inferStateCounter = inferStateCounter inferState + 1}
        return
          (VariableType (TypeVariable (enumId (inferStateCounter inferState)) k)))

inferExplicitlyTypedBindingType
  :: MonadThrow m
  => ClassEnvironment
  -> [TypeSignature Identifier]
  -> (ExplicitlyTypedBinding l)
  -> InferT m ([Predicate], ExplicitlyTypedBinding (TypeSignature l))
inferExplicitlyTypedBindingType ce as (ExplicitlyTypedBinding identifier sc alts) = do
  (Qualified qs t) <- freshInst sc
  (ps, alts') <- inferAltTypes ce as alts t
  s <- InferT (gets inferStateSubstitutions)
  let qs' = map (substitutePredicate s) qs
      t' = substituteType s t
      fs = getTypeVariablesOf getTypeSignatureTypeVariables (map (substituteTypeSignature s) as)
      gs = getTypeTypeVariables t' \\ fs
      sc' = quantify gs (Qualified qs' t')
      ps' = filter (not . entail ce qs') (map (substitutePredicate s) ps)
  (ds, rs) <- split ce fs gs ps'
  if sc /= sc'
    then throwM SignatureTooGeneral
    else if not (null rs)
           then throwM ContextTooWeak
           else return (ds, ExplicitlyTypedBinding identifier sc alts')

inferImplicitlyTypedBindingsTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> [ImplicitlyTypedBinding l]
  -> InferT m ([Predicate], [(TypeSignature Identifier)], [ImplicitlyTypedBinding (TypeSignature l)])
inferImplicitlyTypedBindingsTypes ce as bs = do
  ts <- mapM (\_ -> newVariableType StarKind) bs
  let is = map implicitlyTypedBindingId bs
      scs = map toScheme ts
      as' = zipWith (\x y -> TypeSignature x y) is scs ++ as
  pss0 <-
    sequence
      (zipWith
         (\b t -> inferAltTypes ce as' (implicitlyTypedBindingAlternatives b) t)
         bs
         ts)
  let pss = map fst pss0
      binds' = map snd pss0
  s <- InferT (gets inferStateSubstitutions)
  let ps' = map (substitutePredicate s) (concat pss)
      ts' = map (substituteType s) ts
      fs =
        getTypeVariablesOf
          getTypeSignatureTypeVariables
          (map (substituteTypeSignature s) as)
      vss = map getTypeTypeVariables ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restrictImplicitlyTypedBindings bs
    then let gs' = gs \\ getTypeVariablesOf getPredicateTypeVariables rs
             scs' = map (quantify gs' . (Qualified [])) ts'
         in return
              ( ds ++ rs
              , zipWith (\x y -> TypeSignature x y) is scs'
              , zipWith
                  (\(ImplicitlyTypedBinding l tid _, binds') scheme ->
                     ImplicitlyTypedBinding (TypeSignature l scheme) tid binds')
                  (zip bs binds')
                  scs')
    else let scs' = map (quantify gs . (Qualified rs)) ts'
         in return
              ( ds
              , zipWith (\x y -> TypeSignature x y) is scs'
              , zipWith
                  (\(ImplicitlyTypedBinding l tid _, binds') scheme ->
                     ImplicitlyTypedBinding (TypeSignature l scheme) tid binds')
                  (zip bs binds')
                  scs')

inferBindGroupTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> (BindGroup l)
  -> InferT m ([Predicate], [(TypeSignature Identifier)], BindGroup (TypeSignature l))
inferBindGroupTypes ce as (BindGroup es iss) = do
  let as' = [TypeSignature v sc | ExplicitlyTypedBinding v sc _alts <- es]
  (ps, as'', iss') <-
    inferSequenceTypes0 inferImplicitlyTypedBindingsTypes ce (as' ++ as) iss
  qss <- mapM (inferExplicitlyTypedBindingType ce (as'' ++ as' ++ as)) es
  return (ps ++ concat (map fst qss), as'' ++ as', BindGroup (map snd qss) iss')

inferSequenceTypes0
  :: Monad m
  => (ClassEnvironment -> [(TypeSignature Identifier)] -> [bg l] -> InferT m ([Predicate], [(TypeSignature Identifier)], [bg (TypeSignature l)]))
  -> ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> [[bg l]]
  -> InferT m ([Predicate], [(TypeSignature Identifier)], [[bg (TypeSignature l)]])
inferSequenceTypes0 _ _ _ [] = return ([], [], [])
inferSequenceTypes0 ti ce as (bs:bss) = do
  (ps, as', bs') <- ti ce as bs
  (qs, as'', bss') <- inferSequenceTypes0 ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as', bs' : bss')

inferSequenceTypes
  :: Monad m
  => (ClassEnvironment -> [(TypeSignature Identifier)] -> bg l -> InferT m ([Predicate], [(TypeSignature Identifier)], bg (TypeSignature l)))
  -> ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> [bg l]
  -> InferT m ([Predicate], [(TypeSignature Identifier)], [bg (TypeSignature l)])
inferSequenceTypes _ _ _ [] = return ([], [], [])
inferSequenceTypes ti ce as (bs:bss) = do
  (ps, as', bs') <- ti ce as bs
  (qs, as'', bss') <- inferSequenceTypes ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as', bs' : bss')

--------------------------------------------------------------------------------
-- Instantiation

instantiateType :: [Type] -> Type -> Type
instantiateType ts (ApplicationType l r) =
  ApplicationType (instantiateType ts l) (instantiateType ts r)
instantiateType ts (GenericType n) = ts !! n
instantiateType _ t = t

instantiateQualified :: [Type] -> Qualified Type -> Qualified Type
instantiateQualified ts (Qualified ps t) =
  Qualified (map (instantiatePredicate ts) ps) (instantiateType ts t)

instantiatePredicate :: [Type] -> Predicate -> Predicate
instantiatePredicate ts (IsIn c t) = IsIn c (map (instantiateType ts) t)

--------------------------------------------------------------------------------
-- Type variables

getTypeSignatureTypeVariables :: (TypeSignature Identifier) -> [TypeVariable]
getTypeSignatureTypeVariables = getTypeVariables where
  getTypeVariables (TypeSignature _  scheme) = getSchemeTypeVariables scheme
    where getSchemeTypeVariables (Forall _ qualified) = getQualifiedTypeVariables qualified

getQualifiedTypeVariables :: Qualified Type -> [TypeVariable]
getQualifiedTypeVariables = getTypeVariables
  where
    getTypeVariables (Qualified predicates t) =
      getTypeVariablesOf getPredicateTypeVariables predicates `union`
      getTypeTypeVariables t

getPredicateTypeVariables :: Predicate -> [TypeVariable]
getPredicateTypeVariables (IsIn _ types) = getTypeVariablesOf getTypeTypeVariables types

getTypeTypeVariables :: Type -> [TypeVariable]
getTypeTypeVariables = getTypeVariables where
  getTypeVariables (VariableType typeVariable) = [typeVariable]
  getTypeVariables (ApplicationType type1 type2) =
    getTypeVariables type1 `union` getTypeVariables type2
  getTypeVariables _ = []

getTypeVariablesOf :: (a -> [TypeVariable]) -> [a] -> [TypeVariable]
getTypeVariablesOf f = nub . concatMap f

-- | Get the kind of a type.
typeKind :: Type -> Kind
typeKind (ConstructorType typeConstructor) = typeConstructorKind typeConstructor
typeKind (VariableType typeVariable) = typeVariableKind typeVariable
typeKind (ApplicationType typ _) =
  case (typeKind typ) of
    (FunctionKind _ kind) -> kind

--------------------------------------------------------------------------------
-- GOOD NAMING CONVENInferON, UNSORTED

-- | The monomorphism restriction is invoked when one or more of the
-- entries in a list of implicitly typed bindings is simple, meaning
-- that it has an alternative with no left-hand side patterns. The
-- following function provides a way to test for this:
restrictImplicitlyTypedBindings :: [(ImplicitlyTypedBinding l)] -> Bool
restrictImplicitlyTypedBindings = any simple
  where
    simple =
      any (null . alternativePatterns) . implicitlyTypedBindingAlternatives

-- | The following function calculates the list of ambiguous variables
-- and pairs each one with the list of predicates that must be
-- satisfied by any choice of a default:
ambiguities :: [TypeVariable] -> [Predicate] -> [Ambiguity]
ambiguities typeVariables predicates =
  [ Ambiguity typeVariable (filter (elem typeVariable . getPredicateTypeVariables) predicates)
  | typeVariable <- getTypeVariablesOf getPredicateTypeVariables predicates \\ typeVariables
  ]

-- | The unifyTypeVariable function is used for the special case of unifying a
-- variable u with a type t.
unifyTypeVariable :: MonadThrow m => TypeVariable -> Type -> m [Substitution]
unifyTypeVariable typeVariable typ
  | typ == VariableType typeVariable = return nullSubst
  | typeVariable `elem` getTypeTypeVariables typ = throwM OccursCheckFails
  | typeVariableKind typeVariable /= typeKind typ = throwM KindMismatch
  | otherwise = return [Substitution typeVariable typ]

unifyPredicates :: Predicate -> Predicate -> Maybe [Substitution]
unifyPredicates = lift' unifyTypeList

oneWayMatchPredicate :: Predicate -> Predicate -> Maybe [Substitution]
oneWayMatchPredicate = lift' oneWayMatchLists

unifyTypes :: MonadThrow m => Type -> Type -> m [Substitution]
unifyTypes (ApplicationType l r) (ApplicationType l' r') = do
              s1 <- unifyTypes l l'
              s2 <- unifyTypes (substituteType s1 r) (substituteType s1 r')
              return (s2 @@ s1)
unifyTypes (VariableType u) t = unifyTypeVariable u t
unifyTypes t (VariableType u) = unifyTypeVariable u t
unifyTypes (ConstructorType tc1) (ConstructorType tc2)
              | tc1 == tc2 = return nullSubst
unifyTypes _ _ = throwM TypeMismatch

unifyTypeList :: MonadThrow m => [Type] -> [Type] -> m [Substitution]
unifyTypeList (x:xs) (y:ys) = do
    s1 <- unifyTypes x y
    s2 <- unifyTypeList (map (substituteType s1) xs) (map (substituteType s1) ys)
    return (s2 @@ s1)
unifyTypeList [] [] = return nullSubst
unifyTypeList _ _ = throwM ListsDoNotUnify

oneWayMatchType :: MonadThrow m => Type -> Type -> m [Substitution]
oneWayMatchType (ApplicationType l r) (ApplicationType l' r') = do
  sl <- oneWayMatchType l l'
  sr <- oneWayMatchType r r'
  merge sl sr
oneWayMatchType (VariableType u) t
  | typeVariableKind u == typeKind t = return [Substitution u t]
oneWayMatchType (ConstructorType tc1) (ConstructorType tc2)
  | tc1 == tc2 = return nullSubst
oneWayMatchType _ _ = throwM TypeMismatchOneWay

oneWayMatchLists :: MonadThrow m => [Type] -> [Type] -> m [Substitution]
oneWayMatchLists ts ts' = do
    ss <- sequence (zipWith oneWayMatchType ts ts')
    foldM merge nullSubst ss

--------------------------------------------------------------------------------
-- Garbage

lookupIdentifier
  :: MonadThrow m
  => Identifier -> [(TypeSignature Identifier)] -> m Scheme
lookupIdentifier _ [] = throwM NotInScope
lookupIdentifier i ((TypeSignature i'  sc):as) =
  if i == i'
    then return sc
    else lookupIdentifier i as

enumId :: Int -> Identifier
enumId n = Identifier ("v" ++ show n)

inferLiteralType
  :: Monad m
  => SpecialTypes -> Literal -> InferT m ([Predicate], Type)
inferLiteralType specialTypes (CharacterLiteral _) =
  return ([], specialTypesChar specialTypes)
inferLiteralType _ (IntegerLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Num" [v]], v)
inferLiteralType specialTypes (StringLiteral _) =
  return ([], specialTypesString specialTypes)
inferLiteralType _ (RationalLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Fractional" [v]], v)

inferPattern
  :: MonadThrow m
  => Pattern -> InferT m ([Predicate], [(TypeSignature Identifier)], Type)
inferPattern (VariablePattern i) = do
  v <- newVariableType StarKind
  return ([], [TypeSignature i (toScheme v)], v)
inferPattern WildcardPattern = do
  v <- newVariableType StarKind
  return ([], [], v)
inferPattern (AsPattern i pat) = do
  (ps, as, t) <- inferPattern pat
  return (ps, (TypeSignature i (toScheme t)) : as, t)
inferPattern (LiteralPattern l) = do
  specialTypes <- InferT (gets inferStateSpecialTypes)
  (ps, t) <- inferLiteralType specialTypes l
  return (ps, [], t)
inferPattern (ConstructorPattern (TypeSignature _  sc) pats) = do
  (ps, as, ts) <- inferPatterns pats
  t' <- newVariableType StarKind
  (Qualified qs t) <- freshInst sc
  specialTypes <- InferT (gets inferStateSpecialTypes)
  let makeArrow :: Type -> Type -> Type
      a `makeArrow` b = ApplicationType (ApplicationType (specialTypesFunction specialTypes) a) b
  unify t (foldr makeArrow t' ts)
  return (ps ++ qs, as, t')
-- inferPattern (LazyPattern pat) = inferPattern pat

inferPatterns
  :: MonadThrow m
  => [Pattern] -> InferT m ([Predicate], [(TypeSignature Identifier)], [Type])
inferPatterns pats = do
  psasts <- mapM inferPattern pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  return (ps, as, ts)

predHead :: Predicate -> Identifier
predHead (IsIn i _) = i

lift'
  :: MonadThrow m
  => ([Type] -> [Type] -> m a) -> Predicate -> Predicate -> m a
lift' m (IsIn i ts) (IsIn i' ts')
  | i == i' = m ts ts'
  | otherwise = throwM ClassMismatch

lookupClassTypeVariables :: ClassEnvironment -> Identifier -> [TypeVariable]
lookupClassTypeVariables ce i =
  fromMaybe
    []
    (fmap classTypeVariables (M.lookup i (classEnvironmentClasses ce)))

lookupClassSuperclasses :: ClassEnvironment -> Identifier -> [Predicate]
lookupClassSuperclasses ce i = maybe [] classPredicates (M.lookup i (classEnvironmentClasses ce))

lookupClassInstances :: ClassEnvironment -> Identifier -> [Qualified Predicate]
lookupClassInstances ce i =
  maybe [] classQualifiedPredicates (M.lookup i (classEnvironmentClasses ce))

defined :: Maybe a -> Bool
defined (Just _) = True
defined Nothing = False

modify0 :: ClassEnvironment -> Identifier -> Class -> ClassEnvironment
modify0 ce i c =
  ce {classEnvironmentClasses = M.insert i c (classEnvironmentClasses ce)}

-- | Add a class to the environment. Example:
--
-- @
-- env <- addClass (Identifier \"Num\") [TypeVariable (Identifier \"n\") StarKind] [] mempty
-- @
--
-- Throws 'ReadException' in the case of error.
addClass
  :: MonadThrow m
  => Identifier
  -> [TypeVariable]
  -> [Predicate]
  -> ClassEnvironment
  -> m ClassEnvironment
addClass i vs ps ce
  | defined (M.lookup i (classEnvironmentClasses ce)) = throwM ClassAlreadyDefined
  | any (not . defined . flip M.lookup (classEnvironmentClasses ce) . predHead) ps =
    throwM UndefinedSuperclass
  | otherwise = return (modify0 ce i (Class vs ps []))

-- | Add an instance of a class. Example:
--
-- @
-- env <- addInstance [] (IsIn (Identifier \"Num\") [ConstructorType (TypeConstructor (Identifier \"Integer\") StarKind)]) mempty
-- @
--
-- Throws 'ReadException' in the case of error.
addInstance
  :: MonadThrow m
  => [Predicate] -> Predicate -> ClassEnvironment -> m ClassEnvironment
addInstance ps p@(IsIn i _) ce
  | not (defined (M.lookup i (classEnvironmentClasses ce))) =
    throwM NoSuchClassForInstance
  | any (overlap p) qs = throwM OverlappingInstance
  | otherwise = return (modify0 ce i c)
  where
    its = lookupClassInstances ce i
    qs = [q | (Qualified _ q) <- its]
    c = (Class (lookupClassTypeVariables ce i) (lookupClassSuperclasses ce i) (Qualified ps p : its))

overlap :: Predicate -> Predicate -> Bool
overlap p q = defined (unifyPredicates p q)

bySuper :: ClassEnvironment -> Predicate -> [Predicate]
bySuper ce p@(IsIn i ts) = p : concat (map (bySuper ce) supers)
  where
    supers = map (substitutePredicate substitutions) (lookupClassSuperclasses ce i)
    substitutions = zipWith Substitution (lookupClassTypeVariables ce i) ts

byInst :: ClassEnvironment -> Predicate -> Maybe [Predicate]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- lookupClassInstances ce i]
  where
    tryInst (Qualified ps h) = do
      u <- oneWayMatchPredicate h p
      Just (map (substitutePredicate u) ps)

entail :: ClassEnvironment -> [Predicate] -> Predicate -> Bool
entail ce ps p =
  any (p `elem`) (map (bySuper ce) ps) ||
  case byInst ce p of
    Nothing -> False
    Just qs -> all (entail ce ps) qs

simplify :: ([Predicate] -> Predicate -> Bool) -> [Predicate] -> [Predicate]
simplify ent = loop []
  where
    loop rs [] = rs
    loop rs (p:ps)
      | ent (rs ++ ps) p = loop rs ps
      | otherwise = loop (p : rs) ps

reduce :: ClassEnvironment -> [Predicate] -> [Predicate]
reduce ce = simplify (scEntail ce) . elimTauts ce

elimTauts :: ClassEnvironment -> [Predicate] -> [Predicate]
elimTauts ce ps = [p | p <- ps, not (entail ce [] p)]

scEntail :: ClassEnvironment -> [Predicate] -> Predicate -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

quantify :: [TypeVariable] -> Qualified Type -> Scheme
quantify vs qt = Forall ks (substituteQualified s qt)
  where
    vs' = [v | v <- getQualifiedTypeVariables qt, v `elem` vs]
    ks = map typeVariableKind vs'
    s = zipWith Substitution vs' (map GenericType [0 ..])

toScheme :: Type -> Scheme
toScheme t = Forall [] (Qualified [] t)

merge
  :: MonadThrow m
  => [Substitution] -> [Substitution] -> m [Substitution]
merge s1 s2 =
  if agree
    then return (s1 ++ s2)
    else throwM MergeFail
  where
    agree =
      all
        (\v -> substituteType s1 (VariableType v) == substituteType s2 (VariableType v))
        (map substitutionTypeVariable s1 `intersect`
         map substitutionTypeVariable s2)

inferExpressionType
  :: MonadThrow m
  => ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> (Expression l)
  -> InferT m ([Predicate], Type, Expression (TypeSignature l))
inferExpressionType _ as (VariableExpression l i) = do
  sc <- lookupIdentifier i as
  qualified@(Qualified ps t) <- freshInst sc
  let scheme = (Forall [] qualified)
  return (ps, t, VariableExpression (TypeSignature l scheme) i)
inferExpressionType _ _ (ConstantExpression l s@(TypeSignature _  sc)) = do
  (Qualified ps t) <- freshInst sc
  return (ps, t, (ConstantExpression (TypeSignature l sc) s))
inferExpressionType _ _ (LiteralExpression l0 l) = do
  specialTypes <- InferT (gets inferStateSpecialTypes)
  (ps, t) <- inferLiteralType specialTypes l
  let scheme = (Forall [] (Qualified ps t))
  return (ps, t, LiteralExpression (TypeSignature l0 scheme) l)
inferExpressionType ce as (ApplicationExpression l e f) = do
  (ps, te, e') <- inferExpressionType ce as e
  (qs, tf, f') <- inferExpressionType ce as f
  t <- newVariableType StarKind
  specialTypes <- InferT (gets inferStateSpecialTypes)
  let makeArrow :: Type -> Type -> Type
      a `makeArrow` b = ApplicationType (ApplicationType (specialTypesFunction specialTypes) a) b
  unify (tf `makeArrow` t) te
  let scheme = (Forall [] (Qualified (ps++qs) t))
  return (ps ++ qs, t, ApplicationExpression (TypeSignature l scheme) e' f')
inferExpressionType ce as (LetExpression l bg e) = do
  (ps, as', bg') <- inferBindGroupTypes ce as bg
  (qs, t, e') <- inferExpressionType ce (as' ++ as) e
  let scheme = (Forall [] (Qualified (ps++qs) t))
  return (ps ++ qs, t, LetExpression (TypeSignature l scheme) bg' e')
inferExpressionType ce as (LambdaExpression l alt) = do
  (x, y, s) <- inferAltType ce as alt
  pure
    ( x
    , y
    , LambdaExpression
        (TypeSignature l (typeSignatureScheme (alternativeLabel s)))
        s)
inferExpressionType ce as (IfExpression l e e1 e2) = do
  (ps, t, e') <- inferExpressionType ce as e
  specialTypes <- InferT (gets inferStateSpecialTypes)
  unify t (specialTypesBool specialTypes)
  (ps1, t1, e1') <- inferExpressionType ce as e1
  (ps2, t2, e2') <- inferExpressionType ce as e2
  unify t1 t2
  let scheme = (Forall [] (Qualified (ps ++ ps1 ++ ps2) t1))
  return (ps ++ ps1 ++ ps2, t1, IfExpression (TypeSignature l scheme) e' e1' e2')
inferExpressionType ce as (CaseExpression l e branches) = do
  (ps0, t, e') <- inferExpressionType ce as e
  v <- newVariableType StarKind
  let tiBr (pat, f) = do
        (ps, as', t') <- inferPattern pat
        unify t t'
        (qs, t'', f') <- inferExpressionType ce (as' ++ as) f
        unify v t''
        return (ps ++ qs, (pat, f'))
  branches <- mapM tiBr branches
  let pss = map fst branches
      branches' = map snd branches
  let scheme = (Forall [] (Qualified (ps0 ++ concat pss) v))
  return (ps0 ++ concat pss, v, CaseExpression (TypeSignature l scheme) e' branches')

inferAltType
  :: MonadThrow m
  => ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> Alternative l
  -> InferT m ([Predicate], Type, Alternative (TypeSignature l))
inferAltType ce as (Alternative l pats e) = do
  (ps, as', ts) <- inferPatterns pats
  (qs, t, e') <- inferExpressionType ce (as' ++ as) e
  specialTypes <- InferT (gets inferStateSpecialTypes)
  let makeArrow :: Type -> Type -> Type
      a `makeArrow` b = ApplicationType (ApplicationType (specialTypesFunction specialTypes) a) b
  let scheme = (Forall [] (Qualified (ps ++ qs) (foldr makeArrow t ts)))
  return (ps ++ qs, foldr makeArrow t ts, Alternative (TypeSignature l scheme) pats e')

inferAltTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [(TypeSignature Identifier)]
  -> [Alternative l]
  -> Type
  -> InferT m ([Predicate], [Alternative (TypeSignature l)])
inferAltTypes ce as alts t = do
  psts <- mapM (inferAltType ce as) alts
  mapM_ (unify t) (map snd3 psts)
  return (concat (map fst3 psts), map thd3 psts)
  where snd3 (_,x,_) = x
        thd3 (_,_,x) = x
        fst3 (x,_,_) = x

split
  :: MonadThrow m
  => ClassEnvironment -> [TypeVariable] -> [TypeVariable] -> [Predicate] -> m ([Predicate], [Predicate])
split ce fs gs ps = do
  let ps' = reduce ce ps
      (ds, rs) = partition (all (`elem` fs) . getPredicateTypeVariables) ps'
  rs' <- defaultedPredicates ce (fs ++ gs) rs
  return (ds, rs \\ rs')

candidates :: ClassEnvironment -> Ambiguity -> [Type]
candidates ce (Ambiguity v qs) =
  [ t'
  | let is = [i | IsIn i _ <- qs]
        ts = [t | IsIn _ t <- qs]
  , all ([VariableType v] ==) ts
  , any (`elem` numClasses) is
  , all (`elem` stdClasses) is
  , t' <- []{-classEnvironmentDefaults ce-}
  , all (entail ce []) [IsIn i [t'] | i <- is]
  ]
  where -- disabling these
        numClasses = []
        stdClasses = []

withDefaults
  :: MonadThrow m
  => String->([Ambiguity] -> [Type] -> a) -> ClassEnvironment -> [TypeVariable] -> [Predicate] -> m a
withDefaults label f ce vs ps
  | any null tss = throwM AmbiguousInstance
  | otherwise = do
    return (f vps (map head tss))
  where
    showp :: Show a => a -> String
    showp = \x -> "(" ++ show x ++ ")"
    vps = ambiguities vs ps
    tss = map (candidates ce) vps

defaultedPredicates
  :: MonadThrow m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Predicate]
defaultedPredicates = withDefaults "defaultedPredicates" (\vps _ -> concat (map ambiguityPredicates vps))

defaultSubst
  :: MonadThrow m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Substitution]
defaultSubst = withDefaults "defaultSubst" (\vps ts -> zipWith Substitution (map ambiguityTypeVariable vps) ts)

-- extSubst
--   :: Monad m
--   => [Substitution] -> InferT m ()
-- extSubst s' =
--   InferT
--     (modify
--        (\s -> s {inferStateSubstitutions = s' @@ inferStateSubstitutions s}))

freshInst
  :: Monad m
  => Scheme -> InferT m (Qualified Type)
freshInst (Forall ks qt) = do
  ts <- mapM newVariableType ks
  return (instantiateQualified ts qt)
