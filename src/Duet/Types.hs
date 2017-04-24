{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data types for the project.

module Duet.Types where

import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Function
import           Data.Map.Strict (Map)
import           Data.Monoid
import           Data.String
import           Data.Typeable

data SpecialTypes = SpecialTypes
  { specialTypesBool :: Type
  , specialTypesChar :: Type
  , specialTypesString :: Type
  , specialTypesFunction :: Type
  , specialTypesList :: Type
  , specialTypesInteger :: Type
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
  } deriving (Show, Functor, Traversable, Foldable)

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
  { locationStartLine :: !Int
  , locationStartColumn :: !Int
  , locationEndLine :: !Int
  , locationEndColumn :: !Int
  } deriving (Show)

-- | A Haskell expression.
data Expression l
  = VariableExpression l Identifier
  | LiteralExpression l Literal
  | ConstantExpression l (TypeSignature Identifier)
  | ApplicationExpression l (Expression l) (Expression l)
  | InfixExpression l (Expression l) Identifier (Expression l)
  | LetExpression l (BindGroup l) (Expression l)
  | LambdaExpression l (Alternative l)
  | IfExpression l (Expression l) (Expression l) (Expression l)
  | CaseExpression l (Expression l) [(Pattern, (Expression l))]
  deriving (Show, Functor, Traversable, Foldable)

expressionLabel :: Expression l -> l
expressionLabel =
  \case
     LiteralExpression l _ -> l
     ConstantExpression l _ -> l
     ApplicationExpression l _ _ -> l
     InfixExpression l _ _ _ -> l
     LetExpression l _ _ -> l
     LambdaExpression l _ -> l
     IfExpression l _ _ _ -> l
     CaseExpression l _ _ -> l
     VariableExpression l _ -> l

-- | A pattern match.
data Pattern
  = VariablePattern Identifier
  | WildcardPattern
  | AsPattern Identifier Pattern
  | LiteralPattern Literal
  | ConstructorPattern (TypeSignature Identifier) [Pattern]
--  | LazyPattern Pattern
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
  -- , classEnvironmentDefaults :: ![Type]--Disabling for now because
  -- I don't understand how it works.
  } deriving (Show)

instance Monoid ClassEnvironment where
  mempty = ClassEnvironment mempty {-mempty-}
  mappend x y =
    ClassEnvironment
      (on (<>) classEnvironmentClasses x y)
      {-(on (<>) classEnvironmentDefaults x y)-}

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
