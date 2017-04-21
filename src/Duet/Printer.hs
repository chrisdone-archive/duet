{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Printer where

import Data.List
import Duet.Types

printIdentifier :: Identifier -> String
printIdentifier (Identifier i) = i

printImplicitlyTypedBinding :: ImplicitlyTypedBinding l -> String
printImplicitlyTypedBinding (ImplicitlyTypedBinding _ i [alt]) =
  printIdentifier i ++ " " ++ printAlternative alt

printAlternative :: Alternative l -> [Char]
printAlternative (Alternative _ patterns expression) =
  unwords (map printPattern patterns) ++ " = " ++ printExpression expression

printPattern :: Pattern -> [Char]
printPattern =
  \case
    VariablePattern i -> printIdentifier i
    WildcardPattern -> "_"
    AsPattern i p -> printIdentifier i ++ "@" ++ printPattern p
    LiteralPattern l -> printLiteral l
    ConstructorPattern (TypeSignature i _ _) pats ->
      printIdentifier i ++ " " ++ unwords (map printPattern pats)

-- printTypeSignatureIdent :: SpecialTypes -> (TypeSignature Identifier) -> String
-- printTypeSignatureIdent specialTypes (TypeSignature identifier scheme) =
--   "binding " ++ printIdentifier identifier ++ " :: " ++ printScheme specialTypes scheme

-- printTypeSignatureIdent :: SpecialTypes -> (TypeSignature ) -> String
-- printTypeSignatureExp specialTypes (TypeSignature expression scheme) =
--    "expression " ++ printExpression expression ++ " :: " ++ printScheme specialTypes scheme

printExpression :: (Expression l) -> String
printExpression =
  \case
    LiteralExpression _ l -> printLiteral l
    VariableExpression _ i -> printIdentifier i
    ApplicationExpression _ f x ->
      "(" ++ printExpression f ++ " " ++ printExpression x ++ ")"
    e -> "<TODO>"

printLiteral :: Literal -> String
printLiteral (IntegerLiteral i) = show i
printLiteral (StringLiteral x) = show x
printLiteral l = "<TODO: literal>"

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
