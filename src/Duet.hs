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
  (demo,
  -- * Type checker
  -- $type-checker
    typeCheckModule
  , InferException(..)
  -- * Setting up
  , addClass
  , addInstance
  , defaultSpecialTypes
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
import           Data.Char
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.String
import           Data.Typeable
import           Text.Parsec
import           Text.Parsec.String

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
  let Right xid1 =
        fmap
          (fmap (const ()))
          (parse implicitlyTypedBindingParser "" "onOne f = (f 1)")
  {-print xid1-}
  bindGroups <-
    typeCheckModule
      env'
      [ TypeSignature
          "id"
          (Forall
             [StarKind]
             (Qualified [] (makeArrow (GenericType 0) (GenericType 0))))
      ]
      defaultSpecialTypes
      [ BindGroup
          ([]{-[ ExplicitlyTypedBinding
                 "explicitlyTyped"
                 (Forall
                    [StarKind]
                    (Qualified
                       [IsIn "Num" [(GenericType 0)]]
                       (makeArrow (GenericType 0) (GenericType 0))))
                 [Alternative () [VariablePattern "k"] (VariableExpression () "k")]
           ]-})
          [[ ImplicitlyTypedBinding ()
               "someInt"
               [Alternative () [] (LiteralExpression () (IntegerLiteral 5))]
           ]]
          {-[ [ ImplicitlyTypedBinding ()
                "loop"
                [Alternative () [] (VariableExpression () "loop")]
            , ImplicitlyTypedBinding ()
                "ignoreId"
                [Alternative () [VariablePattern "k"] (VariableExpression () "id")]
            , ImplicitlyTypedBinding ()
                "const"
                [ Alternative ()
                    [VariablePattern "k", VariablePattern "l"]
                    (VariableExpression () "k")
                ]
            , ImplicitlyTypedBinding ()
                "someString"
                [Alternative () [] (LiteralExpression () (StringLiteral "hi"))]
            ]
          , [ ImplicitlyTypedBinding ()
                "someInt"
                [Alternative () [] (LiteralExpression () (IntegerLiteral 5))]
            ]
          , [xid1]
          ]-}
      ]
  mapM_ (putStrLn . show) bindGroups
  where
    tInteger :: Type
    tInteger = ConstructorType (TypeConstructor "Integer" StarKind)
    makeArrow :: Type -> Type -> Type
    a `makeArrow` b =
      ApplicationType
        (ApplicationType (specialTypesFunction defaultSpecialTypes) a)
        b

--------------------------------------------------------------------------------
-- Types

data SpecialTypes = SpecialTypes
  { specialTypesBool :: Type
  , specialTypesChar :: Type
  , specialTypesString :: Type
  , specialTypesFunction :: Type
  , specialTypesList :: Type
  } deriving (Show)

-- | Type inference monad.
newtype InferT m a = InferT
  { runInferT :: StateT InferState m a
  } deriving (Monad, Applicative, Functor, MonadThrow)

-- | State of inferring.
data InferState = InferState
  { inferStateSubstitutions :: ![Substitution]
  , inferStateCounter :: !Int
  , inferStateSpecialTypes :: !SpecialTypes
  -- , inferStateExpressionTypes :: ![(Expression (), Scheme)]
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

-- | Specify the type of @a@.
data TypeSignature a = TypeSignature
  { typeSignatureA :: a
  , typeSignatureScheme :: Scheme
  } deriving (Show)

data BindGroup l = BindGroup
  { bindGroupExplicitlyTypedBindings :: ![(ExplicitlyTypedBinding l)]
  , bindGroupImplicitlyTypedBindings :: ![[(ImplicitlyTypedBinding l)]]
  } deriving (Show, Functor, Traversable, Foldable)

data ImplicitlyTypedBinding l = ImplicitlyTypedBinding
  { implicitlyTypedBindingLabel :: l
  , implicitlyTypedBindingId :: !Identifier
  , implicitlyTypedBindingAlternatives :: ![Alternative l]
  } deriving (Show, Functor, Traversable, Foldable)

-- | The simplest case is for explicitly typed bindings, each of which
-- is described by the name of the function that is being defined, the
-- declared type scheme, and the list of alternatives in its
-- definition.
--
-- Haskell requires that each Alt in the definition of a given
-- identifier has the same number of left-hand side arguments, but we
-- do not need to enforce that here.
data ExplicitlyTypedBinding l = ExplicitlyTypedBinding
  { explicitlyTypedBindingId :: !Identifier
  , explicitlyTypedBindingScheme :: !Scheme
  , explicitlyTypedBindingAlternatives :: ![(Alternative l)]
  } deriving (Show, Functor, Traversable, Foldable)

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
data Alternative l = Alternative
  { alternativeLabel :: l
  , alternativePatterns :: ![Pattern]
  , alternativeExpression :: !(Expression l)
  } deriving (Show, Functor, Traversable, Foldable)

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

data Location = Location
  deriving Show

-- | A Haskell expression.
data Expression l
  = VariableExpression l Identifier
  | LiteralExpression l Literal
  | ConstantExpression l (TypeSignature Identifier)
  | ApplicationExpression l (Expression l) (Expression l)
  | LetExpression l (BindGroup l) (Expression l)
  | LambdaExpression l (Alternative l)
  | IfExpression l (Expression l) (Expression l) (Expression l)
  | CaseExpression l (Expression l) [(Pattern, (Expression l))]
  deriving (Show, Functor, Traversable, Foldable)

-- | A pattern match.
data Pattern
  = VariablePattern Identifier
  | WildcardPattern
  | AsPattern Identifier Pattern
  | LiteralPattern Literal
  | ConstructorPattern (TypeSignature Identifier) [Pattern]
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
-- Parser

implicitlyTypedBindingParser :: Parser (ImplicitlyTypedBinding Location)
implicitlyTypedBindingParser = do
  identifier <- identifierParser
  alternative <- alternativeParser
  pure (ImplicitlyTypedBinding Location identifier [alternative])

identifierParser :: Parser Identifier
identifierParser = fmap Identifier (many1 (satisfy isAlphaNum))

alternativeParser :: Parser (Alternative Location)
alternativeParser = do
  patterns <- many (spaces*>patternParser<*spaces)
  _ <- string "="
  spaces
  expression <- expressionParser
  pure (Alternative Location patterns expression)

patternParser :: Parser Pattern
patternParser = variableParser
  where variableParser = VariablePattern <$> identifierParser

expressionParser :: Parser (Expression Location)
expressionParser = integralParser <|> applicationParser <|> variableParser
  where
    integralParser = (LiteralExpression Location . IntegerLiteral . read) <$> many1 digit
    applicationParser =
      string "(" *>
      (ApplicationExpression Location <$> variableParser <*> (spaces *> expressionParser) <*
       string ")")
    variableParser = VariableExpression Location <$> identifierParser

--------------------------------------------------------------------------------
-- Printer

printIdentifier :: Identifier -> String
printIdentifier (Identifier i) = i

-- printTypeSignatureIdent :: SpecialTypes -> (TypeSignature Identifier) -> String
-- printTypeSignatureIdent specialTypes (TypeSignature identifier scheme) =
--   "binding " ++ printIdentifier identifier ++ " :: " ++ printScheme specialTypes scheme

-- printTypeSignatureIdent :: SpecialTypes -> (TypeSignature ) -> String
-- printTypeSignatureExp specialTypes (TypeSignature expression scheme) =
--    "expression " ++ printExpression expression ++ " :: " ++ printScheme specialTypes scheme

printExpression :: Show l => (Expression l) -> String
printExpression =
  \case
    LiteralExpression _ l -> printLiteral l
    VariableExpression _ i -> printIdentifier i
    ApplicationExpression _ f x ->
      "(" ++ printExpression f ++ " " ++ printExpression x ++ ")"
    e -> show e

printLiteral :: Literal -> String
printLiteral (IntegerLiteral i) = show i
printLiteral (StringLiteral x) = show x
printLiteral l = show l

printScheme :: SpecialTypes -> Scheme -> [Char]
printScheme specialTypes (Forall kinds qualifiedType') =
  (if null kinds
     then ""
     else "forall " ++
          unwords
            (zipWith
               (\i k ->
                  printTypeVariable
                    (TypeVariable (Identifier ("g" ++ show i)) k))
               [0 :: Int ..]
               kinds) ++
          ". ") ++
  printQualifiedType specialTypes qualifiedType'
  where
    printQualifiedType specialTypes (Qualified predicates typ) =
      case predicates of
        [] -> printTypeSansParens specialTypes typ
        _ ->
          "(" ++
          intercalate ", " (map (printPredicate specialTypes) predicates) ++
          ") => " ++ printTypeSansParens specialTypes typ
    printPredicate specialTypes (IsIn identifier types) =
      printIdentifier identifier ++
      " " ++ unwords (map (printType specialTypes) types)

printKind :: Kind -> [Char]
printKind =
  \case
    StarKind -> "*"
    FunctionKind x' y -> printKind x' ++ " -> " ++ printKind y

printTypeSansParens :: SpecialTypes -> Type -> [Char]
printTypeSansParens specialTypes =
  \case
    ApplicationType (ApplicationType func x') y' | func == specialTypesFunction specialTypes ->
      printType specialTypes x' ++ " -> " ++ printTypeSansParens specialTypes y'
    o -> printType specialTypes o

printType :: SpecialTypes -> Type -> [Char]
printType specialTypes =
  \case
    VariableType v -> printTypeVariable v
    ConstructorType tyCon -> printTypeConstructor tyCon
    ApplicationType (ApplicationType func x') y | func == specialTypesFunction specialTypes ->
      "(" ++ printType specialTypes x' ++ " -> " ++ printTypeSansParens specialTypes y ++ ")"
    ApplicationType list ty | list == specialTypesList specialTypes ->
      "[" ++ printTypeSansParens specialTypes ty ++ "]"
    ApplicationType x' y -> "(" ++ printType specialTypes x' ++ " " ++ printType specialTypes y ++ ")"
    GenericType int -> "g" ++ show int
  where printTypeConstructor (TypeConstructor identifier kind) =
          case kind of
            StarKind -> printIdentifier identifier
            _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

printTypeVariable :: TypeVariable -> String
printTypeVariable (TypeVariable identifier kind) =
  case kind of
    StarKind -> printIdentifier identifier
    _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

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
-- Built-in types and classes

-- | Special types that Haskell uses for pattern matching and literals.
defaultSpecialTypes :: SpecialTypes
defaultSpecialTypes =
  SpecialTypes
  { specialTypesBool = ConstructorType (TypeConstructor "Bool" StarKind)
  , specialTypesChar = ConstructorType (TypeConstructor "Char" StarKind)
  , specialTypesString = makeListType (specialTypesChar defaultSpecialTypes)
  , specialTypesFunction =
      ConstructorType
        (TypeConstructor
           "(->)"
           (FunctionKind StarKind (FunctionKind StarKind StarKind)))
  , specialTypesList = listType
  }
  where
    makeListType :: Type -> Type
    makeListType t = ApplicationType listType t
    listType :: Type
    listType =
      ConstructorType (TypeConstructor "[]" (FunctionKind StarKind StarKind))

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
substituteTypeSignature substitutions (TypeSignature identifier scheme) =
    TypeSignature identifier (substituteInScheme substitutions scheme)
  where substituteInScheme substitutions (Forall kinds qualified) =
          Forall kinds (substituteQualified substitutions qualified)
-- substituteTypeSignature substitutions (ExpressionSignature expression scheme) =
--     ExpressionSignature expression (substituteInScheme substitutions scheme)
--   where substituteInScheme substitutions (Forall kinds qualified) =
--           Forall kinds (substituteQualified substitutions qualified)

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
      as' = zipWith TypeSignature is scs ++ as
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
              , zipWith TypeSignature is scs'
              , zipWith
                  (\(ImplicitlyTypedBinding l tid _, binds') scheme ->
                     ImplicitlyTypedBinding (TypeSignature l scheme) tid binds')
                  (zip bs binds')
                  scs')
    else let scs' = map (quantify gs . (Qualified rs)) ts'
         in return
              ( ds
              , zipWith TypeSignature is scs'
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

tellSig
  :: Monad m
  => (Expression l) -> Scheme -> InferT m ()
tellSig ex ty =
  pure ()
  {-InferT
    (modify
       (\s ->
          s
          { inferStateExpressionTypes =
              inferStateExpressionTypes s ++ [(ex, ty)]
          }))-}

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
inferPattern (LazyPattern pat) = inferPattern pat

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
  -- , any (`elem` numClasses) is
  -- , all (`elem` stdClasses) is
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
