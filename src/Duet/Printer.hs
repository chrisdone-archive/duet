{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Printer where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict as M
import           Duet.Types
import           Text.Printf

class Eq a => Printable a where
  printit :: a -> String

instance Printable Name where
  printit =
    \case
      ValueName _ string -> string --  ++ "[value: " ++ show i ++ "]"
      TypeName _ string -> string  --  ++ "[type: " ++ show i ++ "]"
      ConstructorName _ string -> string
      ForallName i -> "g" ++ show i
      DictName i string -> "("  ++ string ++ ":" ++ show i ++")"

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

printAlternative :: (Eq i, Printable i) => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> Alternative i l -> [Char]
printAlternative getType (Alternative _ patterns expression) =
  concat (map (\p->printPattern p ++ " ") patterns) ++ "= " ++ printExpression getType expression

printPattern :: Printable i => Pattern i l -> [Char]
printPattern =
  \case
    VariablePattern _ i -> printIdentifier i
    WildcardPattern _ s -> s
    AsPattern _ i p -> printIdentifier i ++ "@" ++ printPattern p
    LiteralPattern _ l -> printLiteral l
    ConstructorPattern _ i pats ->
      printIdentifier i ++ " " ++ unwords (map printPattern pats)

printExpression :: (Printable i, Eq i) => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpression getType e =
  wrapType
    (case e of
       LiteralExpression _ l -> printLiteral l
       VariableExpression _ i -> printIdentifier i
       ConstantExpression _ i -> printIdentifier i
       ConstructorExpression _ i -> printIdentifier i
       CaseExpression _ e alts ->
         "case " ++
         printExpressionIfPred getType e ++
         " of\n" ++
         intercalate
           "\n"
           (map
              ("  " ++)
              (map
                 (\(p, e') -> printPat p ++ " -> " ++ printExpression getType e')
                 alts))
       ApplicationExpression _ f x ->
         printExpressionAppOp getType f ++
         " " ++ printExpressionAppArg getType x
       LambdaExpression _ (Alternative _ args e) ->
         "\\" ++
         concat (map (\x -> printPattern x ++ " ") args) ++
         "-> " ++ printExpression getType e
       IfExpression _ a b c ->
         "if " ++
         printExpressionIfPred getType a ++
         " then " ++
         printExpression getType b ++ " else " ++ printExpression getType c
       InfixExpression _ f o x ->
         printExpressionAppArg getType f ++
         " " ++ printIdentifier o ++ " " ++ printExpressionAppArg getType x
       _ -> "<TODO>")
  where
    wrapType x =
      case getType (expressionLabel e) of
        (Nothing) -> x
        (Just (specialTypes, TypeSignature _ ty)) ->
          "(" ++ parens x ++ " :: " ++ printScheme specialTypes ty ++ ")"
          where parens k =
                  if any isSpace k
                    then "(" ++ k ++ ")"
                    else k

printPat :: Printable i => Pattern i l -> String
printPat =
  \case
    VariablePattern _ i -> printit i
    ConstructorPattern _ i ps ->
      printit i ++
      (if null ps
         then ""
         else " " ++ unwords (map inner ps))
  where
    inner =
      \case
        VariablePattern _ i -> printit i
        WildcardPattern _ s -> s
        ConstructorPattern _ i ps
          | null ps -> printit i
          | otherwise ->
            "(" ++ printit i ++ " " ++ unwords (map inner ps) ++ ")"

printExpressionAppArg :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpressionAppArg getType=
  \case
    e@(ApplicationExpression {}) -> paren (printExpression getType e)
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(InfixExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e@(CaseExpression {}) -> paren (printExpression getType e)
    e -> printExpression getType e

printExpressionIfPred :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpressionIfPred getType=
  \case
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e@(CaseExpression {}) -> paren (printExpression getType e)
    e -> printExpression getType e

printExpressionAppOp :: Printable i => (l -> Maybe (SpecialTypes i, TypeSignature i ())) -> (Expression i l) -> String
printExpressionAppOp getType=
  \case
    e@(IfExpression {}) -> paren (printExpression getType e)
    e@(LambdaExpression {}) -> paren (printExpression getType e)
    e@(CaseExpression {}) -> paren (printExpression getType e)
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

printClass :: Printable i => SpecialTypes i -> Class i l -> String
printClass specialTypes (Class vars supers instances i methods) =
  "class " ++
  printSupers specialTypes supers ++
  printit i ++
  " " ++
  unwords (map printTypeVariable vars) ++ " where\n  " ++
  intercalate "\n  " (map (printMethod specialTypes) (M.toList methods)) ++
  "\n" ++ intercalate "\n" (map (printInstance specialTypes) instances)

printMethod :: Printable i =>  SpecialTypes i -> (i, Type i) -> String
printMethod specialTypes (i, ty) =
  printit i ++ " :: " ++ printType specialTypes ty

printInstance :: Printable i =>  SpecialTypes i -> Instance i l -> String
printInstance specialTypes (Instance (Qualified predicates typ) _) =
  "instance " ++
  if null predicates
    then printPredicate specialTypes typ
    else printSupers specialTypes predicates ++ printPredicate specialTypes typ

printSupers :: Printable i => SpecialTypes i -> [Predicate Type i] -> [Char]
printSupers specialTypes supers
  | null supers = ""
  | otherwise =
    "(" ++ intercalate ", " (map (printPredicate specialTypes) supers) ++ ") => "

printPredicate :: (Eq i, Printable i) => SpecialTypes i -> Predicate Type i -> [Char]
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
