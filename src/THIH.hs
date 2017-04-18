{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wno-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A clear-to-read, well-documented, implementation of a Haskell 98
-- type checker adapted from Typing Haskell In Haskell, by Mark
-- P. Jones.

module THIH
  (
  -- * Type checker
  -- $type-checker
    typeCheckModule
  , InferException(..)
  -- * Setting up
  , addClass
  , addInstance
  , ClassEnvironment(..)
  , ReadException(..)
  -- * Printers
  , printTypeSignature
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
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.String
import           Data.Typeable

--------------------------------------------------------------------------------
-- Types

-- | Type inference monad.
newtype InferT m a = InferT
  { runInferT :: StateT InferState m a
  } deriving (Monad, Applicative, Functor, MonadThrow)

-- | State of inferring.
data InferState = InferState
  { inferStateSubstitutions :: ![Substitution]
  , inferStateCounter :: !Int
  } deriving (Show)

-- | An exception that may be thrown when reading in source code,
-- before we do any type-checking.
data ReadException
  = ClassAlreadyDefined
  | NoSuchClassForInstance
  | OverlappingInstance
  | UndefinedSuperclass
  deriving (Show, Typeable)
instance Exception ReadException

-- | A type error.
data InferException
  = SignatureTooGeneral
  | ContextTooWeak
  | OccursCheckFails
  | KindMismatch
  | TypeMismatch
  | ListsDoNotUnify
  | TypeMismatchOneWay
  | NotInScope
  | ClassMismatch
  | MergeFail
  | AmbiguousInstance
  deriving (Show, Typeable)
instance Exception InferException

-- | TypeSignatures about the type of a variable are represented by
-- values of this datatype, each of which pairs a variable name with a
-- type scheme.
data TypeSignature = TypeSignature
  { typeSignatureIdentifier :: Identifier
  , typeSignatureScheme :: Scheme
  } deriving (Show)

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
  } deriving (Show)

-- | A single implicitly typed binding is described by a pair
-- containing the name of the variable and a list of alternatives.
-- The monomorphism restriction is invoked when one or more of the
-- entries in a list of implicitly typed bindings is simple, meaning
-- that it has an alternative with no left-hand side patterns.
data ImplicitlyTypedBinding = ImplicitlyTypedBinding
  { implicitlyTypedBindingId :: !Identifier
  , implicitlyTypedBindingAlternatives :: ![Alternative]
  } deriving (Show)

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
  } deriving (Show)

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
  } deriving (Show)

-- | An Alt specifies the left and right hand sides of a function
-- definition. With a more complete syntax for Expr, values of type
-- Alt might also be used in the representation of lambda and case
-- expressions.
data Alternative = Alternative
  { alternativePatterns :: ![Pattern]
  , alternativeExpression :: !Expression
  } deriving (Show)

-- | Substitutions-finite functions, mapping type variables to
-- types-play a major role in type inference.
data Substitution = Substitution
  { substitutionTypeVariable :: !TypeVariable
  , substitutionType :: !Type
  } deriving (Show)

-- | A type variable.
data TypeVariable = TypeVariable
  { typeVariableIdentifier :: !Identifier
  , typeVariableKind :: !Kind
  } deriving (Eq, Show)

-- | An identifier used for variables.
newtype Identifier = Identifier
  { identifierString :: String
  } deriving (Eq, IsString, Ord, Show)

-- | Haskell types can be qualified by adding a (possibly empty) list
-- of predicates, or class constraints, to restrict the ways in which
-- type variables are instantiated.
data Qualified typ = Qualified
  { qualifiedPredicates :: ![Predicate]
  , qualifiedType :: !typ
  } deriving (Eq, Show)

-- | One of potentially many predicates.
data Predicate =
  IsIn Identifier [Type]
  deriving (Eq, Show)

-- | A simple Haskell type.
data Type
  = VariableType TypeVariable
  | ConstructorType TypeConstructor
  | ApplicationType Type Type
  | GenericType Int
  deriving (Eq, Show)

-- | Kind of a type.
data Kind
  = StarKind
  | FunctionKind Kind Kind
  deriving (Eq, Show)

-- | A Haskell expression.
data Expression
  = VariableExpression Identifier
  | LiteralExpression Literal
  | ConstantExpression TypeSignature
  | ApplicationExpression Expression Expression
  | LetExpression BindGroup Expression
  | LambdaExpression Alternative
  | IfExpression Expression Expression Expression
  | CaseExpression Expression [(Pattern, Expression)]
  deriving (Show)

-- | A pattern match.
data Pattern
  = VariablePattern Identifier
  | WildcardPattern
  | AsPattern Identifier Pattern
  | LiteralPattern Literal
  | ConstructorPattern TypeSignature [Pattern]
  | LazyPattern Pattern
  deriving (Show)

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | RationalLiteral Rational
  | StringLiteral String
  deriving (Show)

-- | A class environment.
data ClassEnvironment = ClassEnvironment
  { classEnvironmentClasses :: !(Map Identifier Class)
  , classEnvironmentDefaults :: ![Type]
  } deriving (Show)

instance Monoid ClassEnvironment where
  mempty = ClassEnvironment mempty mempty
  mappend x y =
    ClassEnvironment
      (on (<>) classEnvironmentClasses x y)
      (on (<>) classEnvironmentDefaults x y)

-- | A class.
data Class = Class
  { classTypeVariables :: ![TypeVariable]
  , classPredicates :: ![Predicate]
  , classQualifiedPredicates :: ![Qualified Predicate]
  } deriving (Show)

-- | A type constructor.
data TypeConstructor = TypeConstructor
  { typeConstructorIdentifier :: !Identifier
  , typeConstructorKind :: !Kind
  } deriving (Eq, Show)

-- | A type scheme.
data Scheme =
  Forall [Kind] (Qualified Type)
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Demo (remove later)

demo :: IO ()
demo = do
  env <-
    addClass
      "Num"
      [TypeVariable "n" StarKind]
      []
      mempty {classEnvironmentDefaults = [tInteger]}
  env' <- addInstance [] (IsIn "Num" [tInteger]) env
  assumptions <-
    typeCheckModule
      env'
      [ TypeSignature
          "id"
          (Forall
             [StarKind]
             (Qualified [] (makeArrow (GenericType 0) (GenericType 0))))
      ]
      [ BindGroup
          [ ExplicitlyTypedBinding
              "x"
              (Forall
                 [StarKind]
                 (Qualified
                    [IsIn "Num" [(GenericType 0)]]
                    (makeArrow (GenericType 0) (GenericType 0))))
              [Alternative [VariablePattern "k"] (VariableExpression "k")]
          ]
          [ [ ImplicitlyTypedBinding
                "x"
                [Alternative [] (VariableExpression "x")]
            , ImplicitlyTypedBinding
                "func"
                [Alternative [VariablePattern "k"] (VariableExpression "id")]
            , ImplicitlyTypedBinding
                "func2"
                [ Alternative
                    [VariablePattern "k", VariablePattern "l"]
                    (VariableExpression "k")
                ]
            , ImplicitlyTypedBinding
                "f"
                [Alternative [] (LiteralExpression (StringLiteral "hi"))]
            ]
          , [ ImplicitlyTypedBinding
                "g"
                [Alternative [] (LiteralExpression (IntegerLiteral 5))]
            ]
          ]
      ]
  mapM_ (putStrLn . printTypeSignature) assumptions
  where
    tInteger :: Type
    tInteger = ConstructorType (TypeConstructor "Integer" StarKind)

--------------------------------------------------------------------------------
-- Printer

printIdentifier :: Identifier -> String
printIdentifier (Identifier i) = i

printTypeSignature :: TypeSignature -> String
printTypeSignature (TypeSignature identifier scheme) =
  printIdentifier identifier ++ " :: " ++ printScheme scheme

printScheme :: Scheme -> [Char]
printScheme (Forall kinds qualifiedType') =
  (if null kinds
     then ""
     else "forall " ++
          unwords
            (zipWith
               (\i k ->
                  printTypeVariable
                    (TypeVariable (Identifier ("a" ++ show i)) k))
               [0 :: Int ..]
               kinds) ++
          ". ") ++
  printQualifiedType qualifiedType'

printKind :: Kind -> [Char]
printKind =
  \case
    StarKind -> "*"
    FunctionKind x' y -> printKind x' ++ " -> " ++ printKind y

printQualifiedType :: Qualified Type -> [Char]
printQualifiedType (Qualified predicates typ) =
  case predicates of
    [] -> printTypeSansParens typ
    _ ->
      "(" ++
      intercalate ", " (map printPredicate predicates) ++
      ") => " ++ printTypeSansParens typ

printTypeSansParens :: Type -> [Char]
printTypeSansParens =
  \case
    ApplicationType (ApplicationType (ConstructorType (TypeConstructor (Identifier "(->)") _)) x') y' ->
      printType x' ++ " -> " ++ printTypeSansParens y'
    o -> printType o

printType :: Type -> [Char]
printType =
  \case
    VariableType v -> printTypeVariable v
    ConstructorType tyCon -> printTypeConstructor tyCon
    ApplicationType (ApplicationType (ConstructorType (TypeConstructor (Identifier "(->)") _)) x') y ->
      "(" ++ printType x' ++ " -> " ++ printTypeSansParens y ++ ")"
    ApplicationType (ConstructorType (TypeConstructor (Identifier "[]") _)) ty ->
      "[" ++ printTypeSansParens ty ++ "]"
    ApplicationType x' y -> "(" ++ printType x' ++ " " ++ printType y ++ ")"
    GenericType int -> "a" ++ show int

printTypeConstructor :: TypeConstructor -> String
printTypeConstructor (TypeConstructor identifier kind) =
  case kind of
    StarKind -> printIdentifier identifier
    _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

printTypeVariable :: TypeVariable -> String
printTypeVariable (TypeVariable identifier kind) =
  case kind of
    StarKind -> printIdentifier identifier
    _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

printPredicate :: Predicate -> [Char]
printPredicate (IsIn identifier types) =
  printIdentifier identifier ++ " " ++ unwords (map printType types)

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
  -> [TypeSignature]     -- ^ Pre-defined type signatures e.g. for built-ins or FFI.
  -> [BindGroup]      -- ^ Bindings in the module.
  -> m [TypeSignature]   -- ^ Inferred types for all identifiers.
typeCheckModule ce as bgs =
  evalStateT
    (runInferT $ do
       (ps, as') <- inferSequenceTypes inferBindGroupTypes ce as bgs
       s <- InferT (gets inferStateSubstitutions)
       let rs = reduce ce (map (substitutePredicate s) ps)
       s' <- defaultSubst ce [] rs
       return (map (substituteTypeSignature (s' @@ s)) as'))
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

substituteTypeSignature :: [Substitution] -> TypeSignature -> TypeSignature
substituteTypeSignature substitutions (TypeSignature identifier scheme) =
    TypeSignature identifier (substituteInScheme substitutions scheme)

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

unify :: MonadThrow m => Type -> Type -> InferT m ()
unify t1 t2 = do
  s <- InferT (gets inferStateSubstitutions)
  u <- unifyTypes (substituteType s t1) (substituteType s t2)
  extSubst u

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
  -> [TypeSignature]
  -> ExplicitlyTypedBinding
  -> InferT m [Predicate]
inferExplicitlyTypedBindingType ce as (ExplicitlyTypedBinding _ sc alts) = do
  (Qualified qs t) <- freshInst sc
  ps <- inferAltTypes ce as alts t
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
           else return ds

inferImplicitlyTypedBindingsTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [TypeSignature]
  -> [ImplicitlyTypedBinding]
  -> InferT m ([Predicate], [TypeSignature])
inferImplicitlyTypedBindingsTypes ce as bs = do
  ts <- mapM (\_ -> newVariableType StarKind) bs
  let is = map implicitlyTypedBindingId bs
      scs = map toScheme ts
      as' = zipWith TypeSignature is scs ++ as
      altss = map implicitlyTypedBindingAlternatives bs
  pss <- sequence (zipWith (inferAltTypes ce as') altss ts)
  s <- InferT (gets inferStateSubstitutions)
  let ps' = map (substitutePredicate s) (concat pss)
      ts' = map (substituteType s) ts
      fs = getTypeVariablesOf getTypeSignatureTypeVariables (map (substituteTypeSignature s) as)
      vss = map getTypeTypeVariables ts'
      gs = foldr1 union vss \\ fs
  (ds, rs) <- split ce fs (foldr1 intersect vss) ps'
  if restrictImplicitlyTypedBindings bs
    then let gs' = gs \\ getTypeVariablesOf getPredicateTypeVariables rs
             scs' = map (quantify gs' . (Qualified [])) ts'
         in return (ds ++ rs, zipWith TypeSignature is scs')
    else let scs' = map (quantify gs . (Qualified rs)) ts'
         in return (ds, zipWith TypeSignature is scs')

inferBindGroupTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [TypeSignature]
  -> BindGroup
  -> InferT m ([Predicate], [TypeSignature])
inferBindGroupTypes ce as (BindGroup es iss) = do
  let as' = [TypeSignature v sc | ExplicitlyTypedBinding v sc _alts <- es]
  (ps, as'') <-
    inferSequenceTypes inferImplicitlyTypedBindingsTypes ce (as' ++ as) iss
  qss <- mapM (inferExplicitlyTypedBindingType ce (as'' ++ as' ++ as)) es
  return (ps ++ concat qss, as'' ++ as')

inferSequenceTypes
  :: Monad m
  => (ClassEnvironment -> [TypeSignature] -> bg -> InferT m ([Predicate], [TypeSignature]))
  -> (ClassEnvironment -> [TypeSignature] -> [bg] -> InferT m ([Predicate], [TypeSignature]))
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

getTypeSignatureTypeVariables :: TypeSignature -> [TypeVariable]
getTypeSignatureTypeVariables = getTypeVariables where
  getTypeVariables (TypeSignature _  scheme) = getSchemeTypeVariables scheme

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
  => Identifier -> [TypeSignature] -> m Scheme
lookupIdentifier _ [] = throwM NotInScope
lookupIdentifier i ((TypeSignature i'  sc):as) =
  if i == i'
    then return sc
    else lookupIdentifier i as

enumId :: Int -> Identifier
enumId n = Identifier ("v" ++ show n)

inferLiteralType
  :: Monad m
  => Literal -> InferT m ([Predicate], Type)
inferLiteralType (CharacterLiteral _) = return ([], charType)
inferLiteralType (IntegerLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Num" [v]], v)
inferLiteralType (StringLiteral _) = return ([], stringType)
inferLiteralType (RationalLiteral _) = do
  v <- newVariableType StarKind
  return ([IsIn "Fractional" [v]], v)

inferPattern
  :: MonadThrow m
  => Pattern -> InferT m ([Predicate], [TypeSignature], Type)
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
  (ps, t) <- inferLiteralType l
  return (ps, [], t)
inferPattern (ConstructorPattern (TypeSignature _  sc) pats) = do
  (ps, as, ts) <- inferPatterns pats
  t' <- newVariableType StarKind
  (Qualified qs t) <- freshInst sc
  unify t (foldr makeArrow t' ts)
  return (ps ++ qs, as, t')
inferPattern (LazyPattern pat) = inferPattern pat

inferPatterns
  :: MonadThrow m
  => [Pattern] -> InferT m ([Predicate], [TypeSignature], [Type])
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
  -> [TypeSignature]
  -> Expression
  -> InferT m ([Predicate], Type)
inferExpressionType _ as (VariableExpression i) = do
  sc <- lookupIdentifier i as
  (Qualified ps t) <- freshInst sc
  return (ps, t)
inferExpressionType _ _ (ConstantExpression (TypeSignature _  sc)) = do
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
        (ps, as', t') <- inferPattern pat
        unify t t'
        (qs, t'') <- inferExpressionType ce (as' ++ as) f
        unify v t''
        return (ps ++ qs)
  pss <- mapM tiBr branches
  return (ps0 ++ concat pss, v)

inferAltType
  :: MonadThrow m
  => ClassEnvironment
  -> [TypeSignature]
  -> Alternative
  -> InferT m ([Predicate], Type)
inferAltType ce as (Alternative pats e) = do
  (ps, as', ts) <- inferPatterns pats
  (qs, t) <- inferExpressionType ce (as' ++ as) e
  return (ps ++ qs, foldr makeArrow t ts)

inferAltTypes
  :: MonadThrow m
  => ClassEnvironment
  -> [TypeSignature]
  -> [Alternative]
  -> Type
  -> InferT m [Predicate]
inferAltTypes ce as alts t = do
  psts <- mapM (inferAltType ce as) alts
  mapM_ (unify t) (map snd psts)
  return (concat (map fst psts))

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
  , t' <- classEnvironmentDefaults ce
  , all (entail ce []) [IsIn i [t'] | i <- is]
  ]

withDefaults
  :: MonadThrow m
  => ([Ambiguity] -> [Type] -> a) -> ClassEnvironment -> [TypeVariable] -> [Predicate] -> m a
withDefaults f ce vs ps
  | any null tss = throwM AmbiguousInstance
  | otherwise = return (f vps (map head tss))
  where
    vps = ambiguities vs ps
    tss = map (candidates ce) vps

defaultedPredicates
  :: MonadThrow m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Predicate]
defaultedPredicates = withDefaults (\vps _ -> concat (map ambiguityPredicates vps))

defaultSubst
  :: MonadThrow m
  => ClassEnvironment -> [TypeVariable] -> [Predicate] -> m [Substitution]
defaultSubst = withDefaults (\vps ts -> zipWith Substitution (map ambiguityTypeVariable vps) ts)

extSubst
  :: Monad m
  => [Substitution] -> InferT m ()
extSubst s' =
  InferT
    (modify
       (\s -> s {inferStateSubstitutions = s' @@ inferStateSubstitutions s}))

freshInst
  :: Monad m
  => Scheme -> InferT m (Qualified Type)
freshInst (Forall ks qt) = do
  ts <- mapM newVariableType ks
  return (instantiateQualified ts qt)
