{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A clear-to-read, well-documented, implementation of a Haskell 98
-- type checker adapted from Typing Haskell In Haskell, by Mark
-- P. Jones.

module THIH
  ( typeCheckModule
  , addClass
  , addInstance
  , initialEnv
  , Type(..)
  , Expression(..)
  , Literal(..)
  , Kind(..)
  , Scheme(..)
  , Pattern(..)
  , Assumption(..)
  , ClassEnvironment(..)
  , BindGroup(..)
  , ImplicitlyTypedBinding(..)
  , ExplicitlyTypedBinding(..)
  , Alternative(..)
  , TypeVariable(..)
  , Qualified(..)
  , Class(..)
  , Identifier(..)
  , Predicate(..)
  , TypeConstructor(..)
  ) where

import           Control.Monad.State
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.String

--------------------------------------------------------------------------------
-- Types

-- | Assumptions about the type of a variable are represented by
-- values of the Assump datatype, each of which pairs a variable name
-- with a type scheme.
data Assumption = Assumption
  { assumptionIdentifier :: Identifier
  , assumptionScheme :: Scheme
  }

-- | The first component in each such pair lists any explicitly typed
-- bindings in the group. The second component provides an opportunity
-- to break down the list of any implicitly typed bindings into
-- several smaller lists, arranged in dependency order. In other
-- words, if a binding group is represented by a pair
-- (es,[is_1,...,is_n]), then the implicitly typed bindings in each
-- is_i should depend only on the bindings in es, is_1, ..., is_i, and
-- not on any bindings in is_j when j>i. (Bindings in es could depend
-- on any of the bindings in the group, but will presumably depend on
-- at least those in is_n, or else the group would not be
-- minimal. Note also that if es is empty, then n must be 1.) In
-- choosing this representation, we have assumed that dependency
-- analysis has been carried out prior to type checking, and that the
-- bindings in each group have been organized into values of type
-- BindGroup as appropriate. In particular, by separating out
-- implicitly typed bindings as much as possible, we can potentially
-- increase the degree of polymorphism in inferred types. For a
-- correct implementation of the semantics specified in the Haskell
-- report, a simpler but less flexible approach is required: all
-- implicitly typed bindings must be placed in a single list, even if
-- a more refined decomposition would be possible. In addition, if the
-- group is restricted, then we must also ensure that none of the
-- explicitly typed bindings in the same BindGroup have any predicates
-- in their type, even though this is not strictly necessary. With
-- hindsight, these are restrictions that we might prefer to avoid in
-- any future revision of Haskell.
data BindGroup = BindGroup
  { bindGroupExplicitlyTypedBindings :: ![ExplicitlyTypedBinding]
  , bindGroupImplicitlyTypedBindings :: ![[ImplicitlyTypedBinding]]
  }

-- | A single implicitly typed binding is described by a pair
-- containing the name of the variable and a list of alternatives.
-- The monomorphism restriction is invoked when one or more of the
-- entries in a list of implicitly typed bindings is simple, meaning
-- that it has an alternative with no left-hand side patterns.
data ImplicitlyTypedBinding = ImplicitlyTypedBinding
  { implicitlyTypedBindingId :: !Identifier
  , implicitlyTypedBindingAlternatives :: ![Alternative]
  }

-- | The simplest case is for explicitly typed bindings, each of which
-- is described by the name of the function that is being defined, the
-- declared type scheme, and the list of alternatives in its
-- definition.
--
-- Haskell requires that each Alt in the definition of a given
-- identifier has the same number of left-hand side arguments, but we
-- do not need to enforce that here.
data ExplicitlyTypedBinding = ExplicitlyTypedBinding
  { explicitlyTypedBindingId :: !Identifier
  , explicitlyTypedBindingScheme :: !Scheme
  , explicitlyTypedBindingAlternatives :: ![Alternative]
  }

-- | Suppose, for example, that we are about to qualify a type with a
-- list of predicates ps and that vs lists all known variables, both
-- fixed and generic. An ambiguity occurs precisely if there is a type
-- variable that appears in ps but not in vs (i.e., in tv ps \\
-- vs). The goal of defaulting is to bind each ambiguous type variable
-- v to a monotype t. The type t must be chosen so that all of the
-- predicates in ps that involve v will be satisfied once t has been
-- substituted for v.
data Ambiguity = Ambiguity
  { ambiguityTypeVariable :: !TypeVariable
  , ambiguityPredicates :: ![Predicate]
  }

-- | An Alt specifies the left and right hand sides of a function
-- definition. With a more complete syntax for Expr, values of type
-- Alt might also be used in the representation of lambda and case
-- expressions.
data Alternative = Alternative
  { alternativePatterns :: ![Pattern]
  , alternativeExpression :: !Expression
  }

-- | Substitutions-finite functions, mapping type variables to
-- types-play a major role in type inference.
data Substitution = Substitution
  { substitutionTypeVariable :: !TypeVariable
  , substitutionType :: !Type
  }

-- | A type variable.
data TypeVariable = TypeVariable
  { typeVariableIdentifier :: !Identifier
  , typeVariableKind :: !Kind
  } deriving (Eq)

-- | An identifier used for variables.
newtype Identifier = Identifier
  { identifierString :: String
  } deriving (Eq, IsString, Ord)

-- | Haskell types can be qualified by adding a (possibly empty) list
-- of predicates, or class constraints, to restrict the ways in which
-- type variables are instantiated.
data Qualified typ = Qualified
  { qualifiedPredicates :: ![Predicate]
  , qualifiedType :: !typ
  } deriving (Eq)

-- | One of potentially many predicates.
data Predicate =
  IsIn Identifier [Type]
  deriving (Eq)

-- | A simple Haskell type.
data Type
  = VariableType TypeVariable
  | ConstructorType TypeConstructor
  | ApplicationType Type Type
  | GenericType Int
  deriving (Eq)

-- | Kind of a type.
data Kind
  = StarKind
  | FunctionKind Kind Kind
  deriving (Eq)

-- | A Haskell expression.
data Expression
  = VariableExpression Identifier
  | LiteralExpression Literal
  | ConstantExpression Assumption
  | ApplicationExpression Expression Expression
  | LetExpression BindGroup Expression
  | LambdaExpression Alternative
  | IfExpression Expression Expression Expression
  | CaseExpression Expression [(Pattern, Expression)]

-- | A pattern match.
data Pattern
  = VariablePattern Identifier
  | WildcardPattern
  | AsPattern Identifier Pattern
  | LiteralPattern Literal
  | ConstructorPattern Assumption [Pattern]
  | LazyPattern Pattern

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | RationalLiteral Rational
  | StringLiteral String

-- | A class environment.
data ClassEnvironment = ClassEnvironment
  { classEnvironmentClasses :: !(Map Identifier Class)
  , classEnvironmentDefaults :: ![Type]
  }

-- | A class.
data Class = Class
  { classTypeVariables :: ![TypeVariable]
  , classPredicates :: ![Predicate]
  , classQualifiedPredicates :: ![Qualified Predicate]
  }

-- | A type constructor.
data TypeConstructor = TypeConstructor
  { typeConstructorIdentifier :: !Identifier
  , typeConstructorKind :: !Kind
  } deriving (Eq)

-- | A type scheme.
data Scheme =
  Forall [Kind] (Qualified Type)
  deriving (Eq)

--------------------------------------------------------------------------------
-- Type inference

typeCheckModule :: ClassEnvironment -> [Assumption] -> [BindGroup] -> [Assumption]
typeCheckModule ce as bgs =
  evalState
    (runInfer $ do
       (ps, as') <- inferSequenceTypes inferBindGroupTypes ce as bgs
       s <- Infer (gets inferStateSubstitutions)
       let rs = reduce ce (map (substitutePredicate s) ps)
       s' <- defaultSubst ce [] rs
       return (map (substituteAssumption (s' @@ s)) as'))
    (InferState nullSubst 0)

--------------------------------------------------------------------------------
-- Built-in types and classes

boolType :: Type
boolType = ConstructorType (TypeConstructor "Bool" StarKind)

charType :: Type
charType = ConstructorType (TypeConstructor "Char" StarKind)

stringType :: Type
stringType = makeListType charType

makeListType :: Type -> Type
makeListType t = ApplicationType listType t

listType :: Type
listType = ConstructorType (TypeConstructor "[]" (FunctionKind StarKind StarKind))

makeArrow :: Type -> Type -> Type
a `makeArrow` b = ApplicationType (ApplicationType tArrow a) b

tInteger :: Type
tInteger = ConstructorType (TypeConstructor "Integer" StarKind)

tDouble :: Type
tDouble = ConstructorType (TypeConstructor "Double" StarKind)

tArrow :: Type
tArrow =
  ConstructorType
    (TypeConstructor
       "(->)"
       (FunctionKind StarKind (FunctionKind StarKind StarKind)))

numClasses :: [Identifier]
numClasses =
  ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]

stdClasses :: [Identifier]
stdClasses =
  [ "Eq"
  , "Ord"
  , "Show"
  , "Read"
  , "Bounded"
  , "Enum"
  , "Ix"
  , "Functor"
  , "Monad"
  , "MonadPlus"
  ] ++
  numClasses

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

substituteAssumption :: [Substitution] -> Assumption -> Assumption
substituteAssumption substitutions (Assumption identifier scheme) =
    Assumption identifier (substituteInScheme substitutions scheme)

substitutePredicate :: [Substitution] -> Predicate -> Predicate
substitutePredicate substitutions (IsIn identifier types) =
    IsIn identifier (map (substituteType substitutions) types)

substituteInScheme :: [Substitution] -> Scheme -> Scheme
substituteInScheme substitutions (Forall kinds qualified) =
  Forall kinds (substituteQualified substitutions qualified)

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

-- | Type inferece monad.
newtype Infer a = Infer
  { runInfer :: State InferState a
  } deriving (Monad, Applicative, Functor)

data InferState = InferState
  { inferStateSubstitutions :: ![Substitution]
  , inferStateCounter :: !Int
  }

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  s <- Infer (gets inferStateSubstitutions)
  u <- unifyTypes (substituteType s t1) (substituteType s t2)
  extSubst u

newVariableType :: Kind -> Infer Type
newVariableType k =
  Infer
    (do inferState <- get
        put inferState {inferStateCounter = inferStateCounter inferState + 1}
        return
          (VariableType (TypeVariable (enumId (inferStateCounter inferState)) k)))

inferExplicitlyTypedBindingType :: ClassEnvironment -> [Assumption] -> ExplicitlyTypedBinding -> Infer [Predicate]
inferExplicitlyTypedBindingType ce as (ExplicitlyTypedBinding _ sc alts) = do
  (Qualified qs t) <- freshInst sc
  ps <- inferAltTypes ce as alts t
  s <- Infer (gets inferStateSubstitutions)
  let qs' = map (substitutePredicate s) qs
      t' = substituteType s t
      fs = getTypeVariablesOf getAssumptionTypeVariables (map (substituteAssumption s) as)
      gs = getTypeTypeVariables t' \\ fs
      sc' = quantify gs (Qualified qs' t')
      ps' = filter (not . entail ce qs') (map (substitutePredicate s) ps)
  (ds, rs) <- split ce fs gs ps'
  if sc /= sc'
    then fail "signature too general"
    else if not (null rs)
           then fail "context too weak"
           else return ds

inferImplicitlyTypedBindingsTypes
  :: ClassEnvironment
  -> [Assumption]
  -> [ImplicitlyTypedBinding]
  -> Infer ([Predicate], [Assumption])
inferImplicitlyTypedBindingsTypes ce as bs = do
  ts <- mapM (\_ -> newVariableType StarKind) bs
  let is = map implicitlyTypedBindingId bs
      scs = map toScheme ts
      as' = zipWith Assumption is scs ++ as
      altss = map implicitlyTypedBindingAlternatives bs
  pss <- sequence (zipWith (inferAltTypes ce as') altss ts)
  s <- Infer (gets inferStateSubstitutions)
  let ps' = map (substitutePredicate s) (concat pss)
      ts' = map (substituteType s) ts
      fs = getTypeVariablesOf getAssumptionTypeVariables (map (substituteAssumption s) as)
      vss = map getTypeTypeVariables ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restrictImplicitlyTypedBindings bs
    then let gs' = gs \\ getTypeVariablesOf getPredicateTypeVariables rs
             scs' = map (quantify gs' . (Qualified [])) ts'
         in return (ds ++ rs, zipWith Assumption is scs')
    else let scs' = map (quantify gs . (Qualified rs)) ts'
         in return (ds, zipWith Assumption is scs')

inferBindGroupTypes :: ClassEnvironment
                    -> [Assumption]
                    -> BindGroup
                    -> Infer ([Predicate], [Assumption])
inferBindGroupTypes ce as (BindGroup es iss) = do
  let as' = [Assumption v sc | ExplicitlyTypedBinding v sc _alts <- es]
  (ps, as'') <-
    inferSequenceTypes inferImplicitlyTypedBindingsTypes ce (as' ++ as) iss
  qss <- mapM (inferExplicitlyTypedBindingType ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

inferSequenceTypes
  :: (ClassEnvironment -> [Assumption] -> bg -> Infer ([Predicate], [Assumption]))
  -> (ClassEnvironment -> [Assumption] -> [bg] -> Infer ([Predicate], [Assumption]))
inferSequenceTypes _ _ _ [] = return ([], [])
inferSequenceTypes ti ce as (bs:bss) = do
  (ps, as') <- ti ce as bs
  (qs, as'') <- inferSequenceTypes ti ce (as' ++ as) bss
  return (ps ++ qs, as'' ++ as')

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

getAssumptionTypeVariables :: Assumption -> [TypeVariable]
getAssumptionTypeVariables = getTypeVariables where
  getTypeVariables (Assumption _  scheme) = getSchemeTypeVariables scheme

getQualifiedTypeVariables :: Qualified Type -> [TypeVariable]
getQualifiedTypeVariables = getTypeVariables
  where
    getTypeVariables (Qualified predicates t) =
      getTypeVariablesOf getPredicateTypeVariables predicates `union`
      getTypeTypeVariables t

getPredicateTypeVariables :: Predicate -> [TypeVariable]
getPredicateTypeVariables = getTypeVariables where
  getTypeVariables (IsIn _ types) = getTypeVariablesOf getTypeTypeVariables types

getSchemeTypeVariables :: Scheme -> [TypeVariable]
getSchemeTypeVariables = getTypeVariables where
  getTypeVariables (Forall _ qualified) = getQualifiedTypeVariables qualified

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
restrictImplicitlyTypedBindings :: [ImplicitlyTypedBinding] -> Bool
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
unifyTypeVariable :: Monad m => TypeVariable -> Type -> m [Substitution]
unifyTypeVariable typeVariable typ
  | typ == VariableType typeVariable = return nullSubst
  | typeVariable `elem` getTypeTypeVariables typ = fail "occurs check fails"
  | typeVariableKind typeVariable /= typeKind typ = fail "kinds do not match"
  | otherwise = return [Substitution typeVariable typ]

unifyPredicates :: Predicate -> Predicate -> Maybe [Substitution]
unifyPredicates = lift' unifyTypeList

oneWayMatchPredicate :: Predicate -> Predicate -> Maybe [Substitution]
oneWayMatchPredicate = lift' oneWayMatchLists

unifyTypes :: Monad m => Type -> Type -> m [Substitution]
unifyTypes (ApplicationType l r) (ApplicationType l' r') = do
              s1 <- unifyTypes l l'
              s2 <- unifyTypes (substituteType s1 r) (substituteType s1 r')
              return (s2 @@ s1)
unifyTypes (VariableType u) t = unifyTypeVariable u t
unifyTypes t (VariableType u) = unifyTypeVariable u t
unifyTypes (ConstructorType tc1) (ConstructorType tc2)
              | tc1 == tc2 = return nullSubst
unifyTypes _ _ = fail "types do not unify"

unifyTypeList :: Monad m => [Type] -> [Type] -> m [Substitution]
unifyTypeList (x:xs) (y:ys) = do
    s1 <- unifyTypes x y
    s2 <- unifyTypeList (map (substituteType s1) xs) (map (substituteType s1) ys)
    return (s2 @@ s1)
unifyTypeList [] [] = return nullSubst
unifyTypeList _ _ = fail "lists do not unify"

oneWayMatchType :: Monad m => Type -> Type -> m [Substitution]
oneWayMatchType (ApplicationType l r) (ApplicationType l' r') = do
  sl <- oneWayMatchType l l'
  sr <- oneWayMatchType r r'
  merge sl sr
oneWayMatchType (VariableType u) t
  | typeVariableKind u == typeKind t = return [Substitution u t]
oneWayMatchType (ConstructorType tc1) (ConstructorType tc2)
  | tc1 == tc2 = return nullSubst
oneWayMatchType _ _ = fail "types do not oneWayMatchType"

oneWayMatchLists :: Monad m => [Type] -> [Type] -> m [Substitution]
oneWayMatchLists ts ts' = do
    ss <- sequence (zipWith oneWayMatchType ts ts')
    foldM merge nullSubst ss

--------------------------------------------------------------------------------
-- Garbage

lookupIdentifier
  :: Monad m
  => Identifier -> [Assumption] -> m Scheme
lookupIdentifier i [] = fail ("unbound identifier: " ++ identifierString i)
lookupIdentifier i ((Assumption i'  sc):as) =
  if i == i'
    then return sc
    else lookupIdentifier i as

enumId :: Int -> Identifier
enumId n = Identifier ("v" ++ show n)

inferLiteralType :: Literal -> Infer ([Predicate], Type)
inferLiteralType (CharacterLiteral _) = return ([], charType)
inferLiteralType (IntegerLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Num" [v]], v)
inferLiteralType (StringLiteral _) = return ([], stringType)
inferLiteralType (RationalLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Fractional" [v]], v)

tiPat :: Pattern -> Infer ([Predicate], [Assumption], Type)
tiPat (VariablePattern i) = do
  v <- newVariableType StarKind
  return ([], [Assumption i (toScheme v)], v)
tiPat WildcardPattern = do
  v <- newVariableType StarKind
  return ([], [], v)
tiPat (AsPattern i pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (Assumption i (toScheme t)) : as, t)
tiPat (LiteralPattern l) = do
  (ps, t) <- inferLiteralType l
  return (ps, [], t)
tiPat (ConstructorPattern (Assumption _  sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newVariableType StarKind
  (Qualified qs t) <- freshInst sc
  unify t (foldr makeArrow t' ts)
  return (ps ++ qs, as, t')
tiPat (LazyPattern pat) = tiPat pat

tiPats :: [Pattern] -> Infer ([Predicate], [Assumption], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  return (ps, as, ts)

predHead :: Predicate -> Identifier
predHead (IsIn i _) = i

lift'
  :: Monad m
  => ([Type] -> [Type] -> m a) -> Predicate -> Predicate -> m a
lift' m (IsIn i ts) (IsIn i' ts')
  | i == i' = m ts ts'
  | otherwise = fail "classes differ"

sig :: ClassEnvironment -> Identifier -> [TypeVariable]
sig ce i =
  case M.lookup i (classEnvironmentClasses ce) of
    Just (Class vs _ _) -> vs

super :: ClassEnvironment -> Identifier -> [Predicate]
super ce i =
  case M.lookup i (classEnvironmentClasses ce) of
    Just (Class _ is _) -> is

insts :: ClassEnvironment -> Identifier -> [Qualified Predicate]
insts ce i =
  case M.lookup i (classEnvironmentClasses ce) of
    Just (Class _ _ its) -> its

defined :: Maybe a -> Bool
defined (Just _) = True
defined Nothing = False

modify0 :: ClassEnvironment -> Identifier -> Class -> ClassEnvironment
modify0 ce i c =
  ce {classEnvironmentClasses = M.insert i c (classEnvironmentClasses ce)}

initialEnv :: ClassEnvironment
initialEnv =
  ClassEnvironment
  { classEnvironmentClasses = mempty
  , classEnvironmentDefaults = [tInteger, tDouble]
  }

addClass :: Identifier -> [TypeVariable] -> [Predicate] -> (ClassEnvironment -> Maybe ClassEnvironment)
addClass i vs ps ce
  | defined (M.lookup i (classEnvironmentClasses ce)) = fail "class already defined"
  | any (not . defined . flip M.lookup (classEnvironmentClasses ce) . predHead) ps =
    fail "superclass not defined"
  | otherwise = return (modify0 ce i (Class vs ps []))

addInstance :: [Predicate] -> Predicate -> (ClassEnvironment -> Maybe ClassEnvironment)
addInstance ps p@(IsIn i _) ce
  | not (defined (M.lookup i (classEnvironmentClasses ce))) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = return (modify0 ce i c)
  where
    its = insts ce i
    qs = [q | (Qualified _ q) <- its]
    c = (Class (sig ce i) (super ce i) (Qualified ps p : its))

overlap :: Predicate -> Predicate -> Bool
overlap p q = defined (unifyPredicates p q)

bySuper :: ClassEnvironment -> Predicate -> [Predicate]
bySuper ce p@(IsIn i ts) = p : concat (map (bySuper ce) supers)
  where
    supers = map (substitutePredicate substitutions) (super ce i)
    substitutions = zipWith Substitution (sig ce i) ts

byInst :: ClassEnvironment -> Predicate -> Maybe [Predicate]
byInst ce p@(IsIn i _) = msum [tryInst it | it <- insts ce i]
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
  :: Monad m
  => [Substitution] -> [Substitution] -> m [Substitution]
merge s1 s2 =
  if agree
    then return (s1 ++ s2)
    else fail "merge fails"
  where
    agree =
      all
        (\v -> substituteType s1 (VariableType v) == substituteType s2 (VariableType v))
        (map substitutionTypeVariable s1 `intersect`
         map substitutionTypeVariable s2)

inferExpressionType
  :: ClassEnvironment
  -> [Assumption]
  -> Expression
  -> Infer ([Predicate], Type)
inferExpressionType _ as (VariableExpression i) = do
  sc <- lookupIdentifier i as
  (Qualified ps t) <- freshInst sc
  return (ps, t)
inferExpressionType _ _ (ConstantExpression (Assumption _  sc)) = do
  (Qualified ps t) <- freshInst sc
  return (ps, t)
inferExpressionType _ _ (LiteralExpression l) = do
  (ps, t) <- inferLiteralType l
  return (ps, t)
inferExpressionType ce as (ApplicationExpression e f) = do
  (ps, te) <- inferExpressionType ce as e
  (qs, tf) <- inferExpressionType ce as f
  t <- newVariableType StarKind
  unify (tf `makeArrow` t) te
  return (ps ++ qs, t)
inferExpressionType ce as (LetExpression bg e) = do
  (ps, as') <- inferBindGroupTypes ce as bg
  (qs, t) <- inferExpressionType ce (as' ++ as) e
  return (ps ++ qs, t)
inferExpressionType ce as (LambdaExpression alt) = inferAltType ce as alt
inferExpressionType ce as (IfExpression e e1 e2) = do
  (ps, t) <- inferExpressionType ce as e
  unify t boolType
  (ps1, t1) <- inferExpressionType ce as e1
  (ps2, t2) <- inferExpressionType ce as e2
  unify t1 t2
  return (ps ++ ps1 ++ ps2, t1)
inferExpressionType ce as (CaseExpression e branches) = do
  (ps0, t) <- inferExpressionType ce as e
  v <- newVariableType StarKind
  let tiBr (pat, f) = do
        (ps, as', t') <- tiPat pat
        unify t t'
        (qs, t'') <- inferExpressionType ce (as' ++ as) f
        unify v t''
        return (ps ++ qs)
  pss <- mapM tiBr branches
  return (ps0 ++ concat pss, v)

inferAltType :: ClassEnvironment -> [Assumption] -> Alternative -> Infer ([Predicate], Type)
inferAltType ce as (Alternative pats e) = do
  (ps, as', ts) <- tiPats pats
  (qs, t) <- inferExpressionType ce (as' ++ as) e
  return (ps ++ qs, foldr makeArrow t ts)

inferAltTypes :: ClassEnvironment -> [Assumption] -> [Alternative] -> Type -> Infer [Predicate]
inferAltTypes ce as alts t = do
  psts <- mapM (inferAltType ce as) alts
  mapM_ (unify t) (map snd psts)
  return (concat (map fst psts))

split
  :: Monad m
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
  , t' <- classEnvironmentDefaults ce
  , all (entail ce []) [IsIn i [t'] | i <- is]
  ]

withDefaults
  :: Monad m
  => ([Ambiguity] -> [Type] -> a) -> ClassEnvironment -> [TypeVariable] -> [Predicate] -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise = return (f vps (map head tss))
  where
    vps = ambiguities vs ps
    tss = map (candidates ce) vps

defaultedPredicates
  :: Monad m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Predicate]
defaultedPredicates = withDefaults (\vps _ -> concat (map ambiguityPredicates vps))

defaultSubst
  :: Monad m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Substitution]
defaultSubst = withDefaults (\vps ts -> zipWith Substitution (map ambiguityTypeVariable vps) ts)

extSubst :: [Substitution] -> Infer ()
extSubst s' =
  Infer
    (modify
       (\s -> s {inferStateSubstitutions = s' @@ inferStateSubstitutions s}))

freshInst :: Scheme -> Infer (Qualified Type)
freshInst (Forall ks qt) = do
  ts <- mapM newVariableType ks
  return (instantiateQualified ts qt)
