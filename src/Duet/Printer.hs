{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

-- |

module Duet.Printer where

import           Data.Char
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Duet.Types
import           Text.Printf

class PrintableType (t :: * -> *) where
  printType :: Printable i => Print i l -> SpecialTypes i -> t i -> String

instance PrintableType (Predicate Type) where
  printType = printPredicate

class (Eq a, Identifiable a) => Printable a where
  printit :: Print i l -> a -> String

instance Printable Name where
  printit printer =
    \case
      PrimopName primop -> printPrimop primop
      ValueName i string ->
        string ++
        (if printNameDetails printer
           then "[value:" ++ show i ++ "]"
           else "")
      TypeName i string ->
        string ++
        (if printNameDetails printer
           then "[type:" ++ show i ++ "]"
           else "")
      ConstructorName i string ->
        string ++
        (if printNameDetails printer
           then "[constructor:" ++ show i ++ "]"
           else "")
      ForallName i -> "g" ++ show i
      DictName i string ->
        string ++
        (if printNameDetails printer
           then "[dict:" ++ show i ++ "]"
           else "")
      ClassName i s ->
        s ++
        (if printNameDetails printer
           then "[class:" ++ show i ++ "]"
           else "")
      MethodName i s ->
        s ++
        (if printNameDetails printer
           then "[method:" ++ show i ++ "]"
           else "")

printPrimop :: Primop -> [Char]
printPrimop =
  \case
    PrimopIntegerSubtract -> "subtract"
    PrimopIntegerTimes -> "times"
    PrimopIntegerPlus -> "plus"
    PrimopRationalSubtract -> "subtract"
    PrimopRationalTimes -> "times"
    PrimopRationalPlus -> "plus"
    PrimopRationalDivide -> "divide"
    PrimopStringAppend -> "append"

instance Printable Identifier where
  printit _ =
    \case
      Identifier string -> string

defaultPrint :: Print i b
defaultPrint =
  Print
  { printDictionaries = False
  , printTypes = const Nothing
  , printNameDetails = False
  }

data Print i l = Print
  { printTypes :: (l -> Maybe (SpecialTypes i, TypeSignature Type i ()))
  , printDictionaries :: Bool
  , printNameDetails :: Bool
  }

printDataType :: (Eq i, Printable i , PrintableType t) => Print i l -> SpecialTypes i -> DataType t i -> String
printDataType printer specialTypes (DataType name vars cons) =
  "data " ++ printit printer name ++ " " ++ unwords (map (printTypeVariable printer) vars) ++ "\n  = " ++
    intercalate "\n  | " (map (printConstructor printer specialTypes) cons)

printConstructor :: (Eq i, Printable i, PrintableType t) => Print i l ->  SpecialTypes i -> DataTypeConstructor t i -> [Char]
printConstructor printer specialTypes (DataTypeConstructor name fields) =
  printit printer name ++ " " ++ unwords (map (printType printer specialTypes) fields)

printTypeSignature
  :: (Printable i, Printable j, Eq i)
  => Print i l ->  SpecialTypes i -> TypeSignature Type i j -> String
printTypeSignature printer specialTypes (TypeSignature thing scheme) =
  printit printer thing ++ " :: " ++ printScheme printer specialTypes scheme

printIdentifier :: Printable j => Print i l ->  j -> String
printIdentifier printer = printit printer

printImplicitlyTypedBinding
  :: (Printable i, PrintableType t)
  => Print i l -> ImplicitlyTypedBinding t i l -> String
printImplicitlyTypedBinding printer (ImplicitlyTypedBinding _ i [alt]) =
  printIdentifier printer i ++ " " ++ printAlternative printer alt
printImplicitlyTypedBinding _ _ = ""

printExplicitlyTypedBinding
  :: (Printable i, PrintableType t)
  => Print i l -> SpecialTypes i -> ExplicitlyTypedBinding t i l -> String
printExplicitlyTypedBinding printer specialTypes (ExplicitlyTypedBinding i scheme [alt]) =
  printIdentifier printer i ++ " :: " ++ printScheme printer specialTypes scheme ++ "\n" ++
  printIdentifier printer i ++ " " ++ printAlternative printer alt
printExplicitlyTypedBinding _ _ _ = ""

printAlternative :: (Eq i, Printable i, PrintableType t) => Print i l -> Alternative t i l -> [Char]
printAlternative printer (Alternative _ patterns expression) =
  concat (map (\p->printPattern printer p ++ " ") patterns) ++ "= " ++ printExpression printer expression

printPattern :: (Printable i, PrintableType t) => Print i l ->  Pattern t i l -> [Char]
printPattern printer =
  \case
    VariablePattern _ i -> printIdentifier printer i
    WildcardPattern _ s -> s
    AsPattern _ i p -> printIdentifier printer i ++ "@" ++ printPattern printer p
    LiteralPattern _ l -> printLiteral l
    ConstructorPattern _ i pats ->
      printIdentifier printer i ++ " " ++ unwords (map (printPattern printer) pats)

printExpression :: (Printable i, Eq i, PrintableType t) => Print i l -> (Expression t i l) -> String
printExpression printer e =
  wrapType
    (case e of
       LiteralExpression _ l -> printLiteral l
       VariableExpression _ i -> printIdentifier printer i
       ConstantExpression _ i -> printIdentifier printer i
       ConstructorExpression _ i -> printIdentifier printer i
       CaseExpression _ e alts ->
         "case " ++
         indent 5 (printExpressionIfPred printer e) ++
         " of\n" ++
         indented
           (intercalate
              "\n"
              (map
                 (\(p, e') ->
                    let inner = printExpression printer e'
                    in if any (== '\n') inner
                         then printPat printer p ++ " ->\n" ++ indented inner
                         else printPat printer p ++ " -> " ++ indent 2 inner)
                 alts))
       ApplicationExpression _ f x ->
         case x of
           VariableExpression _ (nonrenamableName -> Just (DictName {}))
             | not (printDictionaries printer) -> printExpressionAppOp printer f
           _ ->
             if any (== '\n') inner || any (== '\n') prefix
               then prefix ++ "\n" ++ indented inner
               else prefix ++ " " ++ indent (length prefix + 1) inner
             where prefix = printExpressionAppOp printer f
                   inner = printExpressionAppArg printer x
       LambdaExpression _ (Alternative _ args e) ->
         if null filteredArgs
            then inner
            else if any (== '\n') inner
                   then "\\" ++ prefix ++ "->\n" ++ indented inner
                   else "\\" ++ prefix ++ "-> " ++ indent (length prefix + 4) inner
         where inner = (printExpression printer e)
               filteredArgs = filter dictPred args
               prefix =
                 concat
                   (map
                      (\x -> printPattern printer x ++ " ")
                      filteredArgs)
               dictPred =
                 if printDictionaries printer
                   then const True
                   else \case
                          VariablePattern _ (nonrenamableName -> Just (DictName {})) ->
                            False
                          _ -> True
       IfExpression _ a b c ->
         "if " ++
         printExpressionIfPred printer a ++
         " then " ++
         printExpression printer b ++ " else " ++ printExpression printer c
       InfixExpression _ f (o, ov) x ->
         printExpressionAppArg printer f ++
         " " ++
         (if printDictionaries printer
             then "`" ++ printExpression printer ov ++ "`"
             else o) ++ " " ++ printExpressionAppArg printer x
       _ -> "<TODO>")
  where
    wrapType x =
      case printTypes printer (expressionLabel e) of
        (Nothing) -> x
        (Just (specialTypes, TypeSignature _ ty)) ->
          "(" ++
          parens x ++ " :: " ++ printScheme printer specialTypes ty ++ ")"
          where parens k =
                  if any isSpace k
                    then "(" ++ k ++ ")"
                    else k

indented :: String -> [Char]
indented x = intercalate "\n" (map ("  "++) (lines x))

indent :: Int -> String -> [Char]
indent n = intercalate ("\n" ++ replicate n ' ') . lines

lined :: [[Char]] -> [Char]
lined = intercalate "\n  "

printPat :: (Printable i, PrintableType t) => Print i l ->  Pattern t i l -> String
printPat printer=
  \case
    VariablePattern _ i -> printit printer i
    ConstructorPattern _ i ps ->
      printit printer i ++
      (if null ps
         then ""
         else " " ++ unwords (map inner ps))
    WildcardPattern{} -> "_"
    AsPattern _ ident p -> printit printer ident ++ "@" ++ printPat printer p
    LiteralPattern _ l -> printLiteral l
  where
    inner =
      \case
        VariablePattern _ i -> printit printer i
        WildcardPattern _ s -> s
        ConstructorPattern _ i ps
          | null ps -> printit printer i
          | otherwise ->
            "(" ++ printit printer i ++ " " ++ unwords (map inner ps) ++ ")"
        AsPattern _ ident p -> printit printer ident ++ "@" ++ printPat printer p
        LiteralPattern _ l -> printLiteral l

printExpressionAppArg :: (Printable i, PrintableType t) => Print i l ->(Expression t i l) -> String
printExpressionAppArg printer =
  \case
    e@(ApplicationExpression {})
      | nodict e -> paren (printExpression printer e)
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(InfixExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e
  where
    nodict =
      \case
        ApplicationExpression _ _ (VariableExpression _ (nonrenamableName -> Just (DictName {})))
          | not (printDictionaries printer) -> False
        _ -> True

printExpressionIfPred :: (Printable i, PrintableType t) => Print i l -> (Expression t i l) -> String
printExpressionIfPred printer=
  \case
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e

printExpressionAppOp :: (Printable i, PrintableType t) => Print i l -> (Expression t i l) -> String
printExpressionAppOp printer=
  \case
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e

paren :: [Char] -> [Char]
paren e = "("  ++ indent 1 e ++ ")"

printLiteral :: Literal -> String
printLiteral (IntegerLiteral i) = show i
printLiteral (RationalLiteral i) = printf "%f" (fromRational i :: Double)
printLiteral (StringLiteral x) = show x
printLiteral (CharacterLiteral x) = show x

printScheme :: (Printable i, Eq i, PrintableType t, PrintableType t1) => Print i l -> SpecialTypes i -> Scheme t i t1 -> [Char]
printScheme printer specialTypes (Forall kinds qualifiedType') =
  (if null kinds
     then ""
     else "forall " ++
          unwords
            (zipWith
               (\_i k ->
                  printTypeVariable
                    (Print
                     { printTypes = const Nothing
                     , printDictionaries = False
                     , printNameDetails = printNameDetails printer
                     })
                    k)
               [0 :: Int ..]
               kinds) ++
          ". ") ++
  printQualifiedType specialTypes qualifiedType'
  where
    printQualifiedType specialTypes (Qualified predicates typ) =
      case predicates of
        [] -> printType printer specialTypes typ
        _ ->
          "(" ++
          intercalate
            ", "
            (map (printPredicate printer specialTypes) predicates) ++
          ") => " ++ printType printer specialTypes typ


printClass :: Printable i => Print i l -> SpecialTypes i -> Class Type i l -> String
printClass printer specialTypes (Class vars supers instances i methods) =
  "class " ++
  printSupers printer specialTypes supers ++
  printit printer i ++
  " " ++
  unwords (map (printTypeVariable printer) vars) ++ " where\n  " ++
  intercalate "\n  " (map (printMethod printer specialTypes) (M.toList methods)) ++
  "\n" ++ intercalate "\n" (map (printInstance printer specialTypes) instances)

printMethod :: Printable i =>  Print i l -> SpecialTypes i -> (i, Scheme Type i Type) -> String
printMethod printer specialTypes (i, scheme) =
  printit printer i ++ " :: " ++ printScheme printer specialTypes scheme

printInstance :: Printable i => Print i l -> SpecialTypes i -> Instance Type i l -> String
printInstance printer specialTypes (Instance scheme _) =
  "instance " ++
  printScheme printer specialTypes scheme

printSupers :: Printable i => Print i l -> SpecialTypes i -> [Predicate Type i] -> [Char]
printSupers printer specialTypes supers
  | null supers = ""
  | otherwise =
    "(" ++ intercalate ", " (map (printPredicate printer specialTypes) supers) ++ ") => "


printPredicate :: (Eq i, Printable i, PrintableType t) => Print i l -> SpecialTypes i -> Predicate t i -> [Char]
printPredicate printer specialTypes (IsIn identifier types) =
  printIdentifier printer identifier ++
  " " ++ unwords (map (wrap . printType printer specialTypes) types)
  where wrap x = if any isSpace x
                    then "(" ++ x ++ ")"
                    else x

printKind :: Kind -> [Char]
printKind =
  \case
    StarKind -> "Type"
    FunctionKind x' y -> printKind x' ++ " -> " ++ printKind y

printTypeSansParens :: (Printable i, Eq i) => Print i l ->  SpecialTypes i -> Type i -> [Char]
printTypeSansParens printer specialTypes =
  \case
    ApplicationType (ApplicationType func x') y'
      | func == ConstructorType (specialTypesFunction specialTypes) ->
        printType printer specialTypes x' ++
        " -> " ++ printTypeSansParens printer specialTypes y'
    o -> printType printer specialTypes o

instance PrintableType Type where
  printType printer specialTypes =
    \case
      VariableType v -> printTypeVariable printer v
      ConstructorType tyCon -> printTypeConstructor printer tyCon
      ApplicationType (ApplicationType func x') y
        | func == ConstructorType (specialTypesFunction specialTypes) ->
          "(" ++
          printType printer specialTypes x' ++
          " -> " ++ printTypeSansParens printer specialTypes y ++ ")"
    -- ApplicationType list ty | list == specialTypesList specialTypes ->
    --   "[" ++ printTypeSansParens specialTypes ty ++ "]"
      ApplicationType x' y ->
        printType printer specialTypes x' ++ " " ++ printTypeArg y
      -- GenericType int -> "g" ++ show int
    where
      printTypeArg =
        \case
          x@ApplicationType {} -> "(" ++ printType printer specialTypes x ++ ")"
          x -> printType printer specialTypes x

instance PrintableType UnkindedType where
  printType printer specialTypes =
    \case
      UnkindedTypeVariable v -> printIdentifier printer v
      UnkindedTypeConstructor tyCon -> printIdentifier printer tyCon
      UnkindedTypeApp x' y ->
        "(" ++ printType printer specialTypes x' ++ " " ++ printType printer specialTypes y ++ ")"

printTypeConstructor :: Printable j => Print i l -> TypeConstructor j -> String
printTypeConstructor printer (TypeConstructor identifier kind) =
  case kind of
    StarKind -> printIdentifier printer identifier
    FunctionKind {} -> printIdentifier printer identifier
        -- _ -> "(" ++ printIdentifier identifier ++ " :: " ++ printKind kind ++ ")"

printTypeVariable :: Printable i => Print i l -> TypeVariable i -> String
printTypeVariable printer (TypeVariable identifier kind) =
  case kind of
    StarKind -> printIdentifier printer identifier
    _ -> "(" ++ printIdentifier printer identifier ++ " :: " ++ printKind kind ++ ")"

curlyQuotes :: [Char] -> [Char]
curlyQuotes t = "‘" <> t <> "’"
