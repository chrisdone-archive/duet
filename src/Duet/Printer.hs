{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Printer where

import Data.List
import Duet.Types
import Text.Printf

class Printable a where
  printit :: a -> String

instance Printable Name where
  printit =
    \case
      ValueName i string -> string --  ++ "[value: " ++ show i ++ "]"
      TypeName i string -> string  --  ++ "[type: " ++ show i ++ "]"
      ForallName i -> "g" ++ show i

instance Printable Identifier where
  printit =
    \case
      Identifier string -> string

printDataType :: (Eq i, Printable i) => SpecialTypes i -> DataType Type i -> String
printDataType specialTypes (DataType name vars cons) =
  "data " ++ printit name ++ " " ++ unwords (map printTypeVariable vars) ++ "\n  = " ++
    intercalate "\n  | " (map (printConstructor specialTypes) cons)

printConstructor :: (Eq i, Printable i) => SpecialTypes i -> DataTypeConstructor Type i -> [Char]
printConstructor specialTypes (DataTypeConstructor name fields) =
  printit name ++ " " ++ unwords (map (printType specialTypes) fields)

printTypeSignature
  :: (Printable i, Printable j, Eq i)
  => SpecialTypes i -> TypeSignature i j -> String
printTypeSignature specialTypes (TypeSignature thing scheme) =
  printit thing ++ " :: " ++ printScheme specialTypes scheme

printIdentifier :: Printable i => i -> String
printIdentifier = printit

printImplicitlyTypedBinding
  :: Printable i => (l -> (Maybe (SpecialTypes i, TypeSignature i ())))
  -> ImplicitlyTypedBinding i l
  -> String
printImplicitlyTypedBinding getType (ImplicitlyTypedBinding _ i [alt]) =
  printIdentifier i ++ " " ++ printAlternative getType alt

printAlternative :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> Alternative i l -> [Char]
printAlternative getType (Alternative _ patterns expression) =
  concat (map (\p->printPattern p ++ " ") patterns) ++ "= " ++ printExpression getType expression

printPattern :: Printable i => Pattern i -> [Char]
printPattern =
  \case
    VariablePattern i -> printIdentifier i
    WildcardPattern -> "_"
    AsPattern i p -> printIdentifier i ++ "@" ++ printPattern p
    LiteralPattern l -> printLiteral l
    ConstructorPattern (TypeSignature i _) pats ->
      printIdentifier i ++ " " ++ unwords (map printPattern pats)

printExpression :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpression getType e =
  wrapType
    (case e of
       LiteralExpression _ l -> printLiteral l
       VariableExpression _ i -> printIdentifier i
       ApplicationExpression _ f x ->
         printExpressionAppOp getType f ++
         " " ++ printExpressionAppArg getType x
       LambdaExpression _ (Alternative _ args e) ->
         "\\" ++
         concat (map (\x -> printPattern x ++ " ") args) ++ "-> " ++ printExpression getType e
       IfExpression _ a b c ->
         "if " ++
         printExpression getType a ++
         " then " ++
         printExpression getType b ++ " else " ++ printExpression getType c
       InfixExpression _ f o x ->
         printExpressionAppArg getType f ++
         " " ++ printIdentifier o ++ " " ++ printExpressionAppArg getType x
       _ -> "<TODO>")
  where
    wrapType = id
    {-wrapType x =
      case getType (expressionLabel e) of
        (Nothing) -> x
        (Just (specialTypes, TypeSignature _ ty)) ->
          "(" ++ x ++ " :: " ++ printScheme specialTypes ty ++ ")"-}

printExpressionAppArg :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpressionAppArg getType=
  \case
    e@(ApplicationExpression {}) -> paren (printExpression getType e)
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(InfixExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e -> printExpression getType e

printExpressionAppOp :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpressionAppOp getType=
  \case
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e -> printExpression getType e

paren :: [Char] -> [Char]
paren e = "("  ++ e ++ ")"

printLiteral :: Literal -> String
printLiteral (IntegerLiteral i) = show i
printLiteral (RationalLiteral i) = printf "%f" (fromRational i :: Double)
printLiteral (StringLiteral x) = show x
printLiteral (CharacterLiteral x) = show x
printLiteral _l = "<TODO: literal>"

printScheme :: (Printable i, Eq i) => SpecialTypes i -> Scheme i -> [Char]
printScheme specialTypes (Forall kinds qualifiedType') =
  (if null kinds
     then ""
     else "forall " ++
          unwords
            (zipWith
               (\i k ->
                  printTypeVariable
                    (TypeVariable (ForallName i) k))
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

printPredicate :: (Eq i, Printable i) => SpecialTypes i -> Predicate i -> [Char]
printPredicate specialTypes (IsIn identifier types) =
  printIdentifier identifier ++
  " " ++ unwords (map (printType specialTypes) types)

printKind :: Kind -> [Char]
printKind =
  \case
    StarKind -> "*"
    FunctionKind x' y -> printKind x' ++ " -> " ++ printKind y

printTypeSansParens :: (Printable i, Eq i) => SpecialTypes i -> Type i -> [Char]
printTypeSansParens specialTypes =
  \case
    ApplicationType (ApplicationType func x') y' | func == specialTypesFunction specialTypes ->
      printType specialTypes x' ++ " -> " ++ printTypeSansParens specialTypes y'
    o -> printType specialTypes o

printType :: (Printable i, Eq i) => SpecialTypes i -> Type i -> [Char]
printType specialTypes =
  \case
    VariableType v -> printTypeVariable v
    ConstructorType tyCon -> printTypeConstructor tyCon
    ApplicationType (ApplicationType func x') y
      | func == specialTypesFunction specialTypes ->
        "(" ++
        printType specialTypes x' ++
        " -> " ++ printTypeSansParens specialTypes y ++ ")"
    -- ApplicationType list ty | list == specialTypesList specialTypes ->
    --   "[" ++ printTypeSansParens specialTypes ty ++ "]"
    ApplicationType x' y -> printType specialTypes x' ++ " " ++ printTypeArg y
    GenericType int -> "g" ++ show int
  where
    printTypeArg =
      \case
        x@ApplicationType {} -> "(" ++ printType specialTypes x ++ ")"
        x -> printType specialTypes x
    printTypeConstructor (TypeConstructor identifier kind) =
      case kind of
        StarKind -> printIdentifier identifier
        FunctionKind {} -> printIdentifier identifier
            -- _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

printTypeVariable :: Printable i => TypeVariable i -> String
printTypeVariable (TypeVariable identifier kind) =
  case kind of
    StarKind -> printIdentifier identifier
    _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"
