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
import           Duet.Types
import           Text.Printf

class (Eq a, Identifiable a) => Printable a where
  printit :: Print i l -> a -> String

instance Printable Name where
  printit printer =
    \case
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
  { printTypes :: (l -> Maybe (SpecialTypes i, TypeSignature i ()))
  , printDictionaries :: Bool
  , printNameDetails :: Bool
  }

printDataType :: (Eq i, Printable i) => Print i l -> SpecialTypes i -> DataType Type i -> String
printDataType printer specialTypes (DataType name vars cons) =
  "data " ++ printit printer name ++ " " ++ unwords (map (printTypeVariable printer) vars) ++ "\n  = " ++
    intercalate "\n  | " (map (printConstructor printer specialTypes) cons)

printConstructor :: (Eq i, Printable i) => Print i l ->  SpecialTypes i -> DataTypeConstructor Type i -> [Char]
printConstructor printer specialTypes (DataTypeConstructor name fields) =
  printit printer name ++ " " ++ unwords (map (printType printer specialTypes) fields)

printTypeSignature
  :: (Printable i, Printable j, Eq i)
  => Print i l ->  SpecialTypes i -> TypeSignature i j -> String
printTypeSignature printer specialTypes (TypeSignature thing scheme) =
  printit printer thing ++ " :: " ++ printScheme printer specialTypes scheme

printIdentifier :: Printable j => Print i l ->  j -> String
printIdentifier printer = printit printer

printImplicitlyTypedBinding
  :: Printable i
  => Print i l -> ImplicitlyTypedBinding i l -> String
printImplicitlyTypedBinding printer (ImplicitlyTypedBinding _ i [alt]) =
  printIdentifier printer i ++ " " ++ printAlternative printer alt

printAlternative :: (Eq i, Printable i) => Print i l -> Alternative i l -> [Char]
printAlternative printer (Alternative _ patterns expression) =
  concat (map (\p->printPattern printer p ++ " ") patterns) ++ "= " ++ printExpression printer expression

printPattern :: Printable i => Print i l ->  Pattern i l -> [Char]
printPattern printer =
  \case
    VariablePattern _ i -> printIdentifier printer i
    WildcardPattern _ s -> s
    AsPattern _ i p -> printIdentifier printer i ++ "@" ++ printPattern printer p
    LiteralPattern _ l -> printLiteral l
    ConstructorPattern _ i pats ->
      printIdentifier printer i ++ " " ++ unwords (map (printPattern printer) pats)

printExpression :: (Printable i, Eq i) => Print i l -> (Expression i l) -> String
printExpression printer e =
  wrapType
    (case e of
       LiteralExpression _ l -> printLiteral l
       VariableExpression _ i -> printIdentifier printer i
       ConstantExpression _ i -> printIdentifier printer i
       ConstructorExpression _ i -> printIdentifier printer i
       CaseExpression _ e alts ->
         "case " ++
         printExpressionIfPred printer e ++
         " of\n" ++
         intercalate
           "\n"
           (map
              ("  " ++)
              (map
                 (\(p, e') ->
                    printPat printer p ++ " -> " ++ printExpression printer e')
                 alts))
       ApplicationExpression _ f x ->
         case x of
           VariableExpression _ (nonrenamableName -> Just (DictName {}))
             | not (printDictionaries printer) -> printExpressionAppOp printer f
           _ ->
             printExpressionAppOp printer f ++
             " " ++ printExpressionAppArg printer x
       LambdaExpression _ (Alternative _ args e) ->
         "\\" ++
         concat (map (\x -> printPattern printer x ++ " ") args) ++
         "-> " ++ printExpression printer e
       IfExpression _ a b c ->
         "if " ++
         printExpressionIfPred printer a ++
         " then " ++
         printExpression printer b ++ " else " ++ printExpression printer c
       InfixExpression _ f o x ->
         printExpressionAppArg printer f ++
         " " ++
         printIdentifier printer o ++ " " ++ printExpressionAppArg printer x
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

printPat :: Printable i => Print i l ->  Pattern i l -> String
printPat printer=
  \case
    VariablePattern _ i -> printit printer i
    ConstructorPattern _ i ps ->
      printit printer i ++
      (if null ps
         then ""
         else " " ++ unwords (map inner ps))
    WildcardPattern{} -> "_"
  where
    inner =
      \case
        VariablePattern _ i -> printit printer i
        WildcardPattern _ s -> s
        ConstructorPattern _ i ps
          | null ps -> printit printer i
          | otherwise ->
            "(" ++ printit printer i ++ " " ++ unwords (map inner ps) ++ ")"

printExpressionAppArg :: Printable i => Print i l ->(Expression i l) -> String
printExpressionAppArg printer=
  \case
    e@(ApplicationExpression {}) -> paren (printExpression printer e)
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(InfixExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e

printExpressionIfPred :: Printable i => Print i l -> (Expression i l) -> String
printExpressionIfPred printer=
  \case
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e

printExpressionAppOp :: Printable i => Print i l -> (Expression i l) -> String
printExpressionAppOp printer=
  \case
    e@(IfExpression {}) -> paren (printExpression printer e)
    e@(LambdaExpression {}) -> paren (printExpression printer e)
    e@(CaseExpression {}) -> paren (printExpression printer e)
    e -> printExpression printer e

paren :: [Char] -> [Char]
paren e = "("  ++ e ++ ")"

printLiteral :: Literal -> String
printLiteral (IntegerLiteral i) = show i
printLiteral (RationalLiteral i) = printf "%f" (fromRational i :: Double)
printLiteral (StringLiteral x) = show x
printLiteral (CharacterLiteral x) = show x

printScheme :: (Printable i, Eq i) => Print i l -> SpecialTypes i -> Scheme i -> [Char]
printScheme printer specialTypes (Forall kinds qualifiedType') =
  (if null kinds
     then ""
     else "forall " ++
          unwords
            (zipWith
               (\i k ->
                  printTypeVariable
                    (Print
                     { printTypes = const Nothing
                     , printDictionaries = False
                     , printNameDetails = printNameDetails printer
                     })
                    (TypeVariable (ForallName i) k))
               [0 :: Int ..]
               kinds) ++
          ". ") ++
  printQualifiedType specialTypes qualifiedType'
  where
    printQualifiedType specialTypes (Qualified predicates typ) =
      case predicates of
        [] -> printTypeSansParens printer specialTypes typ
        _ ->
          "(" ++
          intercalate
            ", "
            (map (printPredicate printer specialTypes) predicates) ++
          ") => " ++ printTypeSansParens printer specialTypes typ
printClass :: Printable i => Print i l -> SpecialTypes i -> Class Type i l -> String
printClass printer specialTypes (Class vars supers instances i methods) =
  "class " ++
  printSupers printer specialTypes supers ++
  printit printer i ++
  " " ++
  unwords (map (printTypeVariable printer) vars) ++ " where\n  " ++
  intercalate "\n  " (map (printMethod printer specialTypes) (M.toList methods)) ++
  "\n" ++ intercalate "\n" (map (printInstance printer specialTypes) instances)

printMethod :: Printable i =>  Print i l -> SpecialTypes i -> (i, ([TypeVariable i], Type i)) -> String
printMethod printer specialTypes (i, (vars, ty)) =
  printit printer i ++ " :: " ++ vars' ++ printType printer specialTypes ty
  where
    vars' =
      if null vars
        then ""
        else "forall " ++ unwords (map (printTypeVariable printer) vars) ++ ". "

printInstance :: Printable i => Print i l -> SpecialTypes i -> Instance Type i l -> String
printInstance printer specialTypes (Instance (Qualified predicates typ) _) =
  "instance " ++
  if null predicates
    then printPredicate printer specialTypes typ
    else printSupers printer specialTypes predicates ++ printPredicate printer specialTypes typ

printSupers :: Printable i => Print i l -> SpecialTypes i -> [Predicate Type i] -> [Char]
printSupers printer specialTypes supers
  | null supers = ""
  | otherwise =
    "(" ++ intercalate ", " (map (printPredicate printer specialTypes) supers) ++ ") => "

printPredicate :: (Eq i, Printable i) => Print i l -> SpecialTypes i -> Predicate Type i -> [Char]
printPredicate printer specialTypes (IsIn identifier types) =
  printIdentifier printer identifier ++
  " " ++ unwords (map (printType printer specialTypes) types)

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

printType :: (Printable i, Eq i) => Print i l ->  SpecialTypes i -> Type i -> [Char]
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
    ApplicationType x' y -> printType printer specialTypes x' ++ " " ++ printTypeArg y
    GenericType int -> "g" ++ show int
  where
    printTypeArg =
      \case
        x@ApplicationType {} -> "(" ++ printType printer specialTypes x ++ ")"
        x -> printType printer specialTypes x

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
