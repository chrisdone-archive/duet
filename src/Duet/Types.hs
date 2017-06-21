{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data types for the project.

module Duet.Types where

import           Text.Parsec (ParseError)
import           Control.Monad.Catch
import           Control.Monad.State
import           Data.Map.Strict (Map)
import           Data.Semigroup
import           Data.String
import           Data.Text (Text)
import           Data.Typeable

-- | A declaration.
data Decl t i l
  = DataDecl (DataType t i)
  | BindGroupDecl (BindGroup t i l)
  | ClassDecl (Class t i l)
  | InstanceDecl (Instance t i l)
  deriving (Show)

-- | Data type.
data DataType t i = DataType
  { dataTypeName :: i
  , dataTypeVariables :: [TypeVariable i]
  , dataTypeConstructors :: [DataTypeConstructor t i]
  } deriving (Show)

dataTypeConstructor :: DataType Type Name -> Type Name
dataTypeConstructor (DataType name vs _) =
  ConstructorType (toTypeConstructor name vs)

toTypeConstructor :: Name -> [TypeVariable Name] -> TypeConstructor Name
toTypeConstructor name vars =
  TypeConstructor name (foldr FunctionKind StarKind (map typeVariableKind vars))

dataTypeToConstructor :: DataType t Name -> TypeConstructor Name
dataTypeToConstructor (DataType name vs _) =
  toTypeConstructor name vs

-- | A data type constructor.
data DataTypeConstructor t i = DataTypeConstructor
  { dataTypeConstructorName :: i
  , dataTypeConstructorFields :: [t i]
  } deriving (Show)

-- | Type for a data typed parsed from user input.
data UnkindedType i
  = UnkindedTypeConstructor i
  | UnkindedTypeVariable i
  | UnkindedTypeApp (UnkindedType i) (UnkindedType i)
  deriving (Show)

-- | Special built-in types you need for type-checking patterns and
-- literals.
data SpecialTypes i = SpecialTypes
  { specialTypesBool       :: DataType Type i
  , specialTypesChar       :: TypeConstructor i
  , specialTypesString     :: TypeConstructor i
  , specialTypesFunction   :: TypeConstructor i
  , specialTypesInteger    :: TypeConstructor i
  , specialTypesRational   :: TypeConstructor i
  } deriving (Show)

-- | Special built-in signatures.
data SpecialSigs i = SpecialSigs
  { specialSigsTrue :: i
  , specialSigsFalse :: i
  , specialSigsPlus :: i
  , specialSigsTimes :: i
  , specialSigsSubtract :: i
  , specialSigsDivide :: i
  } deriving (Show)

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
  | MethodName !Int String
  | PrimopName Primop
  deriving (Show, Eq, Ord)

-- | Pre-defined operations.
data Primop
  = PrimopIntegerPlus
  | PrimopIntegerSubtract
  | PrimopIntegerTimes
  | PrimopRationalDivide
  | PrimopRationalPlus
  | PrimopRationalSubtract
  | PrimopRationalTimes
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | State of inferring.
data InferState = InferState
  { inferStateSubstitutions :: ![Substitution Name]
  , inferStateCounter :: !Int
  , inferStateSpecialTypes :: !(SpecialTypes Name)
  -- , inferStateExpressionTypes :: ![(Expression (), Scheme)]
  } deriving (Show)

data ParseException
  = TokenizerError ParseError
  | ParserError ParseError
 deriving (Typeable, Show)
instance Exception ParseException

data StepException
  = CouldntFindName !Name
  | CouldntFindNameByString !String
  | TypeAtValueScope !Name
  | CouldntFindMethodDict !Name
  deriving (Typeable, Show)
instance Exception StepException

data RenamerException
  = IdentifierNotInVarScope !(Map Identifier Name) !Identifier
  | IdentifierNotInConScope !(Map Identifier Name) !Identifier
  | IdentifierNotInClassScope !(Map Identifier Name) !Identifier
  | IdentifierNotInTypeScope !(Map Identifier Name) !Identifier
  | NameNotInConScope ![TypeSignature Type Name Name] !Name
  | TypeNotInScope ![TypeConstructor Name] !Identifier
  | UnknownTypeVariable ![TypeVariable Name] !Identifier
  | InvalidMethodTypeVariable ![TypeVariable Name] !(TypeVariable Name)
  | KindArgMismatch (Type Name) Kind (Type Name) Kind
  | KindTooManyArgs (Type Name) Kind (Type Name)
  | ConstructorFieldKind Name (Type Name) Kind
  | MustBeStarKind (Type Name) Kind
  | BuiltinNotDefined !String
  | RenamerNameMismatch !Name
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
  = ExplicitTypeMismatch (Scheme Type Name) (Scheme Type Name)
  | ContextTooWeak
  | OccursCheckFails
  | KindMismatch
  | TypeMismatch (Type Name) (Type Name)
  | ListsDoNotUnify
  | TypeMismatchOneWay
  | NotInScope ![TypeSignature Type Name Name] !Name
  | ClassMismatch
  | MergeFail
  | AmbiguousInstance [Ambiguity Name]
  | MissingMethod
  | MissingTypeVar (TypeVariable Name) [(TypeVariable Name, Type Name)]

  deriving (Show, Typeable)
instance Exception InferException

-- | Specify the type of @a@.
data TypeSignature (t :: * -> *) i a = TypeSignature
  { typeSignatureA :: a
  , typeSignatureScheme :: Scheme t i
  } deriving (Show, Functor, Traversable, Foldable, Eq)

data BindGroup (t :: * -> *) i l = BindGroup
  { bindGroupExplicitlyTypedBindings :: ![ExplicitlyTypedBinding t i l]
  , bindGroupImplicitlyTypedBindings :: ![[ImplicitlyTypedBinding t i l]]
  } deriving (Show, Functor, Traversable, Foldable, Eq)

data ImplicitlyTypedBinding (t :: * -> *) i l = ImplicitlyTypedBinding
  { implicitlyTypedBindingLabel :: l
  , implicitlyTypedBindingId :: !i
  , implicitlyTypedBindingAlternatives :: ![Alternative t i l]
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | The simplest case is for explicitly typed bindings, each of which
-- is described by the name of the function that is being defined, the
-- declared type scheme, and the list of alternatives in its
-- definition.
--
-- Haskell requires that each Alt in the definition of a given
-- identifier has the same number of left-hand side arguments, but we
-- do not need to enforce that here.
data ExplicitlyTypedBinding t i l = ExplicitlyTypedBinding
  { explicitlyTypedBindingId :: !i
  , explicitlyTypedBindingScheme :: !(Scheme t i)
  , explicitlyTypedBindingAlternatives :: ![(Alternative t i l)]
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
data Alternative t i l = Alternative
  { alternativeLabel :: l
  , alternativePatterns :: ![Pattern t i l]
  , alternativeExpression :: !(Expression t i l)
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
  } deriving (Ord, Eq, Show)

-- | An identifier used for variables.
newtype Identifier = Identifier
  { identifierString :: String
  } deriving (Eq, IsString, Ord, Show)

-- | Haskell types can be qualified by adding a (possibly empty) list
-- of predicates, or class constraints, to restrict the ways in which
-- type variables are instantiated.
data Qualified t i typ = Qualified
  { qualifiedPredicates :: ![Predicate t i]
  , qualifiedType :: !typ
  } deriving (Eq, Show)

-- | One of potentially many predicates.
data Predicate t i =
  IsIn i [t i]
  deriving (Eq, Show)

-- | A simple Haskell type.
data Type i
  = VariableType (TypeVariable i)
  | ConstructorType (TypeConstructor i)
  | ApplicationType (Type i) (Type i)
  deriving (Eq, Show)

-- | Kind of a type.
data Kind
  = StarKind
  | FunctionKind Kind Kind
  deriving (Eq, Ord, Show)

data Location = Location
  { locationStartLine :: !Int
  , locationStartColumn :: !Int
  , locationEndLine :: !Int
  , locationEndColumn :: !Int
  } deriving (Show, Eq)

-- | A Haskell expression.
data Expression (t :: * -> *) i l
  = VariableExpression l i
  | ConstructorExpression l i
  | ConstantExpression l Identifier
  | LiteralExpression l Literal
  | ApplicationExpression l (Expression t i l) (Expression t i l)
  | InfixExpression l (Expression t i l) (String, Expression t i l) (Expression t i l)
  | LetExpression l (BindGroup t i l) (Expression t i l)
  | LambdaExpression l (Alternative t i l)
  | IfExpression l (Expression t i l) (Expression t i l) (Expression t i l)
  | CaseExpression l (Expression t i l) [(Pattern t i l, (Expression t i l))]
  deriving (Show, Functor, Traversable, Foldable, Eq)

expressionLabel :: Expression t i l -> l
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
data Pattern (t :: * -> *) i l
  = VariablePattern l i
  | WildcardPattern l String
  | AsPattern l i (Pattern t i l)
  | LiteralPattern l Literal
  | ConstructorPattern l i [Pattern t i l]
--  | LazyPattern Pattern
  deriving (Show , Eq , Functor, Traversable, Foldable)

patternLabel :: Pattern ty t t1 -> t1
patternLabel (VariablePattern loc _) = loc
patternLabel (ConstructorPattern loc _ _) = loc
patternLabel (WildcardPattern l _) = l
patternLabel (AsPattern l  _ _) = l
patternLabel (LiteralPattern l _) =l

data Literal
  = IntegerLiteral Integer
  | CharacterLiteral Char
  | RationalLiteral Rational
  | StringLiteral String
  deriving (Show, Eq)

-- | A class.
data Class (t :: * -> *) i l = Class
  { classTypeVariables :: ![TypeVariable i]
  , classSuperclasses :: ![Predicate t i]
  , classInstances :: ![Instance t i l]
  , className :: i
  , classMethods :: Map i (Scheme t i)
  } deriving (Show)

-- | Class instance.
data Instance (t :: * -> *) i l = Instance
  { instancePredicate :: !(Qualified t i (Predicate t i))
  , instanceDictionary :: !(Dictionary t i l)
  } deriving (Show)

instanceClassName :: Instance t1 i t -> i
instanceClassName (Instance (Qualified _ (IsIn x _)) _) = x

-- | A dictionary for a class.
data Dictionary (t :: * -> *) i l = Dictionary
  { dictionaryName :: i
  , dictionaryMethods :: Map i (Alternative t i l)
  } deriving (Show, Functor, Traversable, Foldable, Eq)

-- | A type constructor.
data TypeConstructor i = TypeConstructor
  { typeConstructorIdentifier :: !i
  , typeConstructorKind :: !Kind
  } deriving (Eq, Show)

-- | A type scheme.
data Scheme t i =
  Forall [TypeVariable i] (Qualified t i (t i))
  deriving (Eq, Show)

data Result a
  = OK a
  | Fail
  deriving (Show, Functor)

instance Semigroup a => Semigroup (Result a) where
  Fail <> _ = Fail
  _ <> Fail = Fail
  OK x <> OK y = OK (x <> y)

data Match t i l
  = Success [(i, Expression t i l)]
  | NeedsMoreEval [Int]
  deriving (Eq, Show, Functor)

instance Semigroup (Match t i l) where
  NeedsMoreEval is <> _ = NeedsMoreEval is
  _ <> NeedsMoreEval is = NeedsMoreEval is
  Success xs <> Success ys = Success (xs <> ys)

class Identifiable i where
  identifyValue :: MonadThrow m => i -> m Identifier
  identifyType :: MonadThrow m => i -> m Identifier
  identifyClass :: MonadThrow m => i -> m Identifier
  nonrenamableName :: i -> Maybe Name

instance Identifiable Identifier where
  identifyValue = pure
  identifyType = pure
  identifyClass = pure
  nonrenamableName _ = Nothing

instance Identifiable Name where
  identifyValue =
    \case
      ValueName _ i -> pure (Identifier i)
      ConstructorName _ c -> pure (Identifier c)
      DictName _ i -> pure (Identifier i)
      MethodName _ i -> pure (Identifier i)
      PrimopName {} -> error "identifyValue PrimopName"
      n -> throwM (TypeAtValueScope n)
  identifyType =
    \case
      TypeName _ i -> pure (Identifier i)
      n -> throwM (RenamerNameMismatch n)
  identifyClass =
    \case
      ClassName _ i -> pure (Identifier i)
      n -> throwM (RenamerNameMismatch n)
  nonrenamableName n =
    case n of
      ValueName {} -> Nothing
      ConstructorName {} -> pure n
      TypeName {} -> pure n
      ForallName {} -> pure n
      DictName {} -> pure n
      ClassName {} -> pure n
      MethodName {} -> pure n
      PrimopName {} -> pure n

-- | Context for the type checker.
data Context t i l = Context
  { contextSpecialSigs :: SpecialSigs i
  , contextSpecialTypes :: SpecialTypes i
  , contextSignatures :: [TypeSignature t i i]
  , contextScope :: Map Identifier i
  , contextTypeClasses :: Map i (Class t i (TypeSignature t i l))
  , contextDataTypes :: [DataType t i]
  } deriving (Show)

-- | Builtin context.
data Builtins t i l = Builtins
  { builtinsSpecialSigs :: SpecialSigs i
  , builtinsSpecialTypes :: SpecialTypes i
  , builtinsSignatures :: [TypeSignature t i i]
  , builtinsTypeClasses :: Map i (Class t i l)
  } deriving (Show)

data Token
  = If
  | Imply
  | Then
  | Data
  | ForallToken
  | Else
  | Case
  | Where
  | Of
  | Backslash
  | Let
  | In
  | RightArrow
  | OpenParen
  | CloseParen
  | Equals
  | Colons
  | Variable !Text
  | Constructor !Text
  | Character !Char
  | String !Text
  | Operator !Text
  | Period
  | Comma
  | Integer !Integer
  | Decimal !Double
  | NonIndentedNewline
  | Bar
  | ClassToken
  | InstanceToken
  deriving (Eq, Ord)

data Specials n = Specials
  { specialsSigs :: SpecialSigs n
  , specialsTypes :: SpecialTypes n
  }
