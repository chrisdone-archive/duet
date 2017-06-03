{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data types for the project.

module Duet.Types where

import Control.Monad.Catch
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Semigroup
import Data.String
import Data.Typeable

-- | A declaration.
data Decl f i l
  = DataDecl (DataType f i)
  | BindGroupDecl (BindGroup i l)
  | ClassDecl (Class f i l)
  | InstanceDecl (Instance f i l)
  deriving (Show)

-- | Data type.
data DataType f i = DataType
  { dataTypeName :: i
  , dataTypeVariables :: [TypeVariable i]
  , dataTypeConstructors :: [DataTypeConstructor f i]
  } deriving (Show)

dataTypeConstructor :: DataType Type Name -> Type Name
dataTypeConstructor (DataType name vs _) =
  ConstructorType (toTypeConstructor name vs)

toTypeConstructor :: Name -> [TypeVariable Name] -> TypeConstructor Name
toTypeConstructor name vars =
  TypeConstructor name (foldr FunctionKind StarKind (map typeVariableKind vars))

-- | A data type constructor.
data DataTypeConstructor f i = DataTypeConstructor
  { dataTypeConstructorName :: i
  , dataTypeConstructorFields :: [f i]
  } deriving (Show)

-- | Type for a data typed parsed from user input.
data ParsedType i
  = ParsedTypeConstructor i
  | ParsedTypeVariable i
  | ParsedTypeApp (ParsedType i) (ParsedType i)
  deriving (Show)

-- | Special built-in types you need for type-checking patterns and
-- literals.
data SpecialTypes i = SpecialTypes
  { specialTypesBool       :: DataType Type i
  , specialTypesChar       :: Type i
  , specialTypesString     :: Type i
  , specialTypesFunction   :: Type i
  , specialTypesInteger    :: Type i
  , specialTypesRational   :: Type i
  , specialTypesShow       :: i
  } deriving (Show)

-- | Special built-in signatures.
data SpecialSigs i = SpecialSigs
  { specialSigsTrue :: i
  , specialSigsFalse :: i
  , specialSigsShow :: i
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
  | ConstructorName !Int !String
  | TypeName !Int !String
  | ForallName !Int
  | DictName !Int String
  | ClassName !Int String
  deriving (Show, Eq, Ord)

-- | State of inferring.
data InferState = InferState
  { inferStateSubstitutions :: ![Substitution Name]
  , inferStateCounter :: !Int
  , inferStateSpecialTypes :: !(SpecialTypes Name)
  -- , inferStateExpressionTypes :: ![(Expression (), Scheme)]
  } deriving (Show)

data StepException
  = CouldntFindName !Name
  | CouldntFindNameByString !String
  | TypeAtValueScope !Name
  deriving (Typeable, Show)
instance Exception StepException

data RenamerException
  = IdentifierNotInVarScope !(Map Identifier Name) !Identifier
  | IdentifierNotInConScope !(Map Identifier Name) !Identifier
  | NameNotInConScope ![TypeSignature Name Name] !Name
  | TypeNotInScope ![TypeConstructor Name] !Identifier
  | RenamerKindMismatch (Type Name) Kind (Type Name) Kind
  | KindTooManyArgs (Type Name) Kind (Type Name)
  | ConstructorFieldKind Name (Type Name) Kind
  | BuiltinNotDefined !String
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

data ResolveException =
  NoInstanceFor (Predicate Type Name)
  deriving (Show, Typeable)
instance Exception ResolveException

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
  { bindGroupExplicitlyTypedBindings :: ![ExplicitlyTypedBinding i l]
  , bindGroupImplicitlyTypedBindings :: ![[ImplicitlyTypedBinding i l]]
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
  , ambiguityPredicates :: ![Predicate Type i]
  } deriving (Show)

-- | An Alt specifies the left and right hand sides of a function
-- definition. With a more complete syntax for Expr, values of type
-- Alt might also be used in the representation of lambda and case
-- expressions.
data Alternative i l = Alternative
  { alternativeLabel :: l
  , alternativePatterns :: ![Pattern i l]
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
data Qualified f i typ = Qualified
  { qualifiedPredicates :: ![Predicate f i]
  , qualifiedType :: !typ
  } deriving (Eq, Show)

-- | One of potentially many predicates.
data Predicate f i =
  IsIn i [f i]
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
  | ConstructorExpression l i
  | ConstantExpression l Identifier
  | LiteralExpression l Literal
  | ApplicationExpression l (Expression i l) (Expression i l)
  | InfixExpression l (Expression i l) i (Expression i l)
  | LetExpression l (BindGroup i l) (Expression i l)
  | LambdaExpression l (Alternative i l)
  | IfExpression l (Expression i l) (Expression i l) (Expression i l)
  | CaseExpression l (Expression i l) [(Pattern i l, (Expression i l))]
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
     ConstructorExpression l _ -> l

-- | A pattern match.
data Pattern i l
  = VariablePattern l i
  | WildcardPattern l String
  | AsPattern l i (Pattern i l)
  | LiteralPattern l Literal
  | ConstructorPattern l i [Pattern i l]
--  | LazyPattern Pattern
  deriving (Show , Eq , Functor, Traversable, Foldable)

patternLabel :: Pattern t t1 -> t1
patternLabel (VariablePattern loc _) = loc
patternLabel (ConstructorPattern loc _ _) = loc
patternLabel (WildcardPattern l _) = l

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | RationalLiteral Rational
  | StringLiteral String
  deriving (Show , Eq)

-- | A class.
data Class f i l = Class
  { classTypeVariables :: ![TypeVariable i]
  , classSuperclasses :: ![Predicate f i]
  , classInstances :: ![Instance f i l]
  , className :: i
  , classMethods :: Map i (f i)
  } deriving (Show)

-- | Class instance.
data Instance f i l = Instance
  { instancePredicate :: !(Qualified f i (Predicate f i))
  , instanceDictionary :: !(Dictionary i l)
  } deriving (Show)

-- | A dictionary for a class.
data Dictionary i l = Dictionary
  { dictionaryName :: i
  , dictionaryMethods :: Map i (Alternative i l)
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | A type constructor.
data TypeConstructor i = TypeConstructor
  { typeConstructorIdentifier :: !i
  , typeConstructorKind :: !Kind
  } deriving (Eq, Show)

-- | A type scheme.
data Scheme i =
  Forall [Kind] (Qualified Type i (Type i))
  deriving (Eq, Show)

data Result a
  = OK a
  | Fail
  deriving (Show, Functor)

instance Semigroup a => Semigroup (Result a) where
  Fail <> _ = Fail
  _ <> Fail = Fail
  OK x <> OK y = OK (x <> y)

data Match i l
  = Success [(i, Expression i l)]
  | NeedsMoreEval [Int]
  deriving (Eq, Show, Functor)

instance Semigroup (Match i l) where
  NeedsMoreEval is <> _ = NeedsMoreEval is
  _ <> NeedsMoreEval is = NeedsMoreEval is
  Success xs <> Success ys = Success (xs <> ys)
