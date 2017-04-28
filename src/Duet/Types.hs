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

-- | Data type.
data DataType f i = DataType
  { dataTypeName :: i
  , dataTypeVariables :: [i]
  , dataTypeConstructors :: [DataTypeConstructor f i]
  }

-- | A data type constructor.
data DataTypeConstructor f i = DataTypeConstructor
  { dataTypeConstructorName :: i
  , dataTypeConstructorFields :: [f i]
  }

-- | Type for a data type field.
data FieldType i
  = FieldTypeConstructor i
  | FieldTypeVariable i
  | FieldTypeApp (FieldType i) (FieldType i)

-- | Special built-in types you need for type-checking patterns and
-- literals.
data SpecialTypes i = SpecialTypes
  { specialTypesBool       :: Type i
  , specialTypesChar       :: Type i
  , specialTypesString     :: Type i
  , specialTypesFunction   :: Type i
  , specialTypesInteger    :: Type i
  , specialTypesNum        :: i
  , specialTypesFractional :: i
  } deriving (Show)

-- | Special built-in signatures.
data SpecialSigs i = SpecialSigs
  { specialSigsTrue :: i
  , specialSigsFalse :: i
  }

-- | Type inference monad.
newtype InferT m a = InferT
  { runInferT :: StateT InferState m a
  } deriving (Monad, Applicative, Functor, MonadThrow)

-- | Name is a globally unique identifier for any thing. No claim
-- about "existence", but definitely uniquness. A name names one thing
-- and one thing only.
--
-- So this comes /after/ the parsing step, and /before/ the
-- type-checking step. The renamer's job is to go from Identifier -> Name.
data Name
  = ValueName !Int !String
  | TypeName !Int !String
  | ForallName !Int
  deriving (Show, Eq, Ord)

-- | State of inferring.
data InferState = InferState
  { inferStateSubstitutions :: ![Substitution Name]
  , inferStateCounter :: !Int
  , inferStateSpecialTypes :: !(SpecialTypes Name)
  -- , inferStateExpressionTypes :: ![(Expression (), Scheme)]
  } deriving (Show)

data RenamerException
  = IdentifierNotInScope !(Map Identifier Name) !Identifier
  | TypeNotInScope ![Name] !Name
  deriving (Show, Typeable)
instance Exception RenamerException

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
  | TypeMismatch (Type Name) (Type Name)
  | ListsDoNotUnify
  | TypeMismatchOneWay
  | NotInScope ![TypeSignature Name Name] !Name
  | ClassMismatch
  | MergeFail
  | AmbiguousInstance [Ambiguity Name]
  deriving (Show, Typeable)
instance Exception InferException

-- | Specify the type of @a@.
data TypeSignature i a = TypeSignature
  { typeSignatureA :: a
  , typeSignatureScheme :: Scheme i
  } deriving (Show, Functor, Traversable, Foldable, Eq)

data BindGroup i l = BindGroup
  { bindGroupExplicitlyTypedBindings :: ![(ExplicitlyTypedBinding i l)]
  , bindGroupImplicitlyTypedBindings :: ![[(ImplicitlyTypedBinding i l)]]
  } deriving (Show, Functor, Traversable, Foldable, Eq)

data ImplicitlyTypedBinding i l = ImplicitlyTypedBinding
  { implicitlyTypedBindingLabel :: l
  , implicitlyTypedBindingId :: !i
  , implicitlyTypedBindingAlternatives :: ![Alternative i l]
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | The simplest case is for explicitly typed bindings, each of which
-- is described by the name of the function that is being defined, the
-- declared type scheme, and the list of alternatives in its
-- definition.
--
-- Haskell requires that each Alt in the definition of a given
-- identifier has the same number of left-hand side arguments, but we
-- do not need to enforce that here.
data ExplicitlyTypedBinding i l = ExplicitlyTypedBinding
  { explicitlyTypedBindingId :: !i
  , explicitlyTypedBindingScheme :: !(Scheme i)
  , explicitlyTypedBindingAlternatives :: ![(Alternative i l)]
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | Suppose, for example, that we are about to qualify a type with a
-- list of predicates ps and that vs lists all known variables, both
-- fixed and generic. An ambiguity occurs precisely if there is a type
-- variable that appears in ps but not in vs (i.e., in tv ps \\
-- vs). The goal of defaulting is to bind each ambiguous type variable
-- v to a monotype t. The type t must be chosen so that all of the
-- predicates in ps that involve v will be satisfied once t has been
-- substituted for v.
data Ambiguity i = Ambiguity
  { ambiguityTypeVariable :: !(TypeVariable i)
  , ambiguityPredicates :: ![Predicate i]
  } deriving (Show)

-- | An Alt specifies the left and right hand sides of a function
-- definition. With a more complete syntax for Expr, values of type
-- Alt might also be used in the representation of lambda and case
-- expressions.
data Alternative i l = Alternative
  { alternativeLabel :: l
  , alternativePatterns :: ![Pattern i]
  , alternativeExpression :: !(Expression i l)
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | Substitutions-finite functions, mapping type variables to
-- types-play a major role in type inference.
data Substitution i = Substitution
  { substitutionTypeVariable :: !(TypeVariable i)
  , substitutionType :: !(Type i)
  } deriving (Show)

-- | A type variable.
data TypeVariable i = TypeVariable
  { typeVariableIdentifier :: !i
  , typeVariableKind :: !Kind
  } deriving (Eq, Show)

-- | An identifier used for variables.
newtype Identifier = Identifier
  { identifierString :: String
  } deriving (Eq, IsString, Ord, Show)

-- | Haskell types can be qualified by adding a (possibly empty) list
-- of predicates, or class constraints, to restrict the ways in which
-- type variables are instantiated.
data Qualified i typ = Qualified
  { qualifiedPredicates :: ![Predicate i]
  , qualifiedType :: !typ
  } deriving (Eq, Show)

-- | One of potentially many predicates.
data Predicate i =
  IsIn i [Type i]
  deriving (Eq, Show)

-- | A simple Haskell type.
data Type i
  = VariableType (TypeVariable i)
  | ConstructorType (TypeConstructor i)
  | ApplicationType (Type i) (Type i)
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
  } deriving (Show, Eq)

-- | A Haskell expression.
data Expression i l
  = VariableExpression l i
  | LiteralExpression l Literal
  | ConstantExpression l (TypeSignature i i)
  | ApplicationExpression l (Expression i l) (Expression i l)
  | InfixExpression l (Expression i l) i (Expression i l)
  | LetExpression l (BindGroup i l) (Expression i l)
  | LambdaExpression l (Alternative i l)
  | IfExpression l (Expression i l) (Expression i l) (Expression i l)
  | CaseExpression l (Expression i l) [(Pattern i, (Expression i l))]
  deriving (Show, Functor, Traversable, Foldable, Eq)

expressionLabel :: Expression i l -> l
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
data Pattern i
  = VariablePattern i
  | WildcardPattern
  | AsPattern i (Pattern i)
  | LiteralPattern Literal
  | ConstructorPattern (TypeSignature i i) [Pattern i]
--  | LazyPattern Pattern
  deriving (Show , Eq)

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | RationalLiteral Rational
  | StringLiteral String
  deriving (Show , Eq)

-- | A class environment.
data ClassEnvironment i = ClassEnvironment
  { classEnvironmentClasses :: !(Map i (Class i))
  -- , classEnvironmentDefaults :: ![Type]--Disabling for now because
  -- I don't understand how it works.
  } deriving (Show)

instance (Ord i) => Monoid (ClassEnvironment i) where
  mempty = ClassEnvironment mempty
  mappend x y =
    ClassEnvironment
      (on (<>) classEnvironmentClasses x y)

-- | A class.
data Class i = Class
  { classTypeVariables :: ![TypeVariable i]
  , classPredicates :: ![Predicate i]
  , classQualifiedPredicates :: ![Qualified i (Predicate i)]
  } deriving (Show)

-- | A type constructor.
data TypeConstructor i = TypeConstructor
  { typeConstructorIdentifier :: !i
  , typeConstructorKind :: !Kind
  } deriving (Eq, Show)

-- | A type scheme.
data Scheme i =
  Forall [Kind] (Qualified i (Type i))
  deriving (Eq, Show)
