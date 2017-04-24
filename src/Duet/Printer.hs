{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Printer where

import Data.List
import Duet.Types
import Text.Printf

printIdentifier :: Identifier -> String
printIdentifier (Identifier i) = i

printImplicitlyTypedBinding
  :: (l -> (Maybe (SpecialTypes, TypeSignature ())))
  -> ImplicitlyTypedBinding l
  -> String
printImplicitlyTypedBinding getType (ImplicitlyTypedBinding _ i [alt]) =
  printIdentifier i ++ " " ++ printAlternative getType alt

printAlternative :: (forall a. l -> Maybe (SpecialTypes, TypeSignature ())) -> Alternative l -> [Char]
printAlternative getType (Alternative _ patterns expression) =
  concat (map (\p->printPattern p ++ " ") patterns) ++ "= " ++ printExpression getType expression

printPattern :: Pattern -> [Char]
printPattern =
  \case
    VariablePattern i -> printIdentifier i
    WildcardPattern -> "_"
    AsPattern i p -> printIdentifier i ++ "@" ++ printPattern p
    LiteralPattern l -> printLiteral l
    ConstructorPattern (TypeSignature i _) pats ->
      printIdentifier i ++ " " ++ unwords (map printPattern pats)

printExpression :: (forall a. l -> Maybe (SpecialTypes, TypeSignature ())) -> (Expression l) -> String
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

printExpressionAppArg :: (forall a. l -> Maybe (SpecialTypes, TypeSignature ())) -> (Expression l) -> String
printExpressionAppArg getType=
  \case
    e@(ApplicationExpression {}) -> paren (printExpression getType e)
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(InfixExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e -> printExpression getType e

printExpressionAppOp :: (forall a. l -> Maybe (SpecialTypes, TypeSignature ())) -> (Expression l) -> String
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
