{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Parser where

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Duet.Printer
import           Duet.Tokenizer
import           Duet.Types
import           Text.Parsec hiding (satisfy, anyToken)

parseFile :: FilePath -> IO (Either ParseError [Decl ParsedType Identifier Location])
parseFile fp = do t <- T.readFile fp
                  return (parseText fp t)

parseText :: SourceName -> Text -> Either ParseError [Decl ParsedType Identifier Location]
parseText fp inp =
  case parse tokensTokenizer fp (inp) of
    Left e -> Left e
    Right tokens' ->
      case runParser tokensParser 0 fp tokens' of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: TokenParser [Decl ParsedType Identifier Location]
tokensParser = moduleParser <* endOfTokens

moduleParser :: TokenParser [Decl ParsedType Identifier Location]
moduleParser =
  many
    ((fmap (\x -> BindGroupDecl (BindGroup [] [[x]])) varfundecl) <|>
     fmap DataDecl datadecl <|>
     fmap ClassDecl classdecl <|>
     fmap InstanceDecl instancedecl)

classdecl :: TokenParser (Class ParsedType Identifier Location)
classdecl =
  go <?> "class declaration (e.g. class Show a where show a :: a -> String)"
  where
    go = do
      u <- getState
      loc <- equalToken ClassToken
      setState (locationStartColumn loc)
      (c, _) <-
        consumeToken
          (\case
             Constructor c -> Just c
             _ -> Nothing) <?> "new class name e.g. Show"
      vars <- many1 typeVariableP
      mwhere <-
        fmap (const True) (equalToken Where) <|> fmap (const False) endOfDecl
      methods <-
        if mwhere
          then do
            (_, identLoc) <-
              lookAhead
                (consumeToken
                   (\case
                      Variable i -> Just i
                      _ -> Nothing)) <?> "class methods e.g. foo :: a -> Int"
            (many1 (methodParser (locationStartColumn identLoc))) <*
              endOfDecl
          else (pure [])
      setState u
      _ <- (pure () <* satisfyToken (==NonIndentedNewline)) <|> endOfTokens
      pure
        (Class
         { className = Identifier (T.unpack c)
         , classTypeVariables = vars
         , classSuperclasses = []
         , classInstances = []
         , classMethods = M.fromList methods
         })
      where
        endOfDecl =
          (pure () <* satisfyToken (== NonIndentedNewline)) <|> endOfTokens
        typeVariableP = go' <?> "type variable (e.g. ‘a’, ‘f’, etc.)"
          where
            go' = do
              (v, _) <-
                consumeToken
                  (\case
                     Variable i -> Just i
                     _ -> Nothing)
              pure (TypeVariable (Identifier (T.unpack v)) StarKind)
        methodParser startCol = go' <?> "method signature e.g. foo :: a -> Y"
          where
            go' = do
              u <- getState
              (v, p) <-
                consumeToken
                  (\case
                     Variable i -> Just i
                     _ -> Nothing)
              when
                (locationStartColumn p /= startCol)
                (unexpected
                   ("method name at column " ++
                    show (locationStartColumn p) ++
                    ", it should start at column " ++ show startCol ++
                    " to match the others"))
              setState startCol
              _ <- equalToken Colons <?> "‘::’ for method signature"
              ty <- parsedType <?> "method type signature e.g. foo :: Int"
              setState u
              pure (Identifier (T.unpack v), ty)

instancedecl :: TokenParser (Instance ParsedType Identifier Location)
instancedecl =
  go <?> "instance declaration (e.g. instance Show Int where show = ...)"
  where
    go = do
      u <- getState
      loc <- equalToken InstanceToken
      setState (locationStartColumn loc)
      (c, _) <-
        consumeToken
          (\case
             Constructor c -> Just c
             _ -> Nothing) <?>
        "class name e.g. Show"
      ty <- parsedType
      mwhere <-
        fmap (const True) (equalToken Where) <|> fmap (const False) endOfDecl
      methods <-
        if mwhere
          then do
            (_, identLoc) <-
              lookAhead
                (consumeToken
                   (\case
                      Variable i -> Just i
                      _ -> Nothing)) <?>
              "instance methods e.g. foo :: a -> Int"
            (many1 (methodParser (locationStartColumn identLoc))) <* endOfDecl
          else (pure [])
      setState u
      _ <- (pure () <* satisfyToken (== NonIndentedNewline)) <|> endOfTokens
      let predicate = Qualified [] (IsIn (Identifier (T.unpack c)) [ty])
          dictName = "$dict" ++ T.unpack c
      pure
        (Instance
         { instancePredicate = predicate
         , instanceDictionary =
             Dictionary (Identifier dictName) (M.fromList methods)
         })
      where
        endOfDecl =
          (pure () <* satisfyToken (== NonIndentedNewline)) <|> endOfTokens
        methodParser startCol =
          go' <?> "method implementation e.g. foo = \\x -> f x"
          where
            go' = do
              u <- getState
              (v, p) <-
                consumeToken
                  (\case
                     Variable i -> Just i
                     _ -> Nothing)
              when
                (locationStartColumn p /= startCol)
                (unexpected
                   ("method name at column " ++
                    show (locationStartColumn p) ++
                    ", it should start at column " ++
                    show startCol ++ " to match the others"))
              setState startCol
              _ <- equalToken Equals <?> "‘=’ for method declaration e.g. x = 1"
              e <- expParser
              setState u
              pure (Identifier (T.unpack v), makeAlt (expressionLabel e) e)

parseType :: TokenParser (Type Identifier)
parseType = infix' <|> app <|> unambiguous
  where
    infix' = do
      left <- (app <|> unambiguous) <?> "left-hand side of function arrow"
      tok <- fmap Just (operator <?> ("function arrow " ++ curlyQuotes "->")) <|> pure Nothing
      case tok of
        Just (RightArrow, _) -> do
          right <-
            parseType <?>
            ("right-hand side of function arrow " ++ curlyQuotes "->")
          pure
            (ApplicationType
               (ApplicationType
                  (ConstructorType
                     (TypeConstructor
                        (Identifier "(->)")
                        (FunctionKind StarKind StarKind)))
                  left)
               right)
        _ -> pure left
      where
        operator =
          satisfyToken
            (\case
               RightArrow {} -> True
               _ -> False)
    app = do
      f <- unambiguous
      args <- many unambiguous
      pure (foldl' ApplicationType f args)
    unambiguous = atomicType <|> parensTy parseType
    atomicType = consParse <|> varParse
    consParse = do
      (v, _) <-
        consumeToken
          (\case
             Constructor i -> Just i
             _ -> Nothing) <?>
        "type constructor (e.g. Int, Maybe)"
      pure
        (ConstructorType (TypeConstructor (Identifier (T.unpack v)) StarKind))
    varParse = do
      (v, _) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing) <?>
        "type variable (e.g. a, f)"
      pure (VariableType (TypeVariable (Identifier (T.unpack v)) StarKind))
    parensTy p = go <?> "parentheses e.g. (T a)"
      where
        go = do
          _ <- equalToken OpenParen
          e <- p <?> "type inside parentheses e.g. (Maybe a)"
          _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
          pure e

datadecl :: TokenParser (DataType ParsedType Identifier)
datadecl = go <?> "data declaration (e.g. data Maybe a = Just a | Nothing)"
  where
    tyvar = unkinded <|> kinded
      where
        kinded =
          kparens
            (do t <- unkinded
                _ <- equalToken Colons
                k <- kindParser
                pure (TypeVariable (typeVariableIdentifier t) k))
          where
            kparens :: TokenParser a -> TokenParser a
            kparens p = g <?> "parens e.g. (x)"
              where
                g = do
                  _ <- equalToken OpenParen
                  e <-
                    p <?> "type with kind inside parentheses e.g. (t :: Type)"
                  _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
                  pure e
        unkinded = do
          (v, _) <-
            consumeToken
              (\case
                 Variable i -> Just i
                 _ -> Nothing) <?>
            "variable name"
          pure (TypeVariable (Identifier (T.unpack v)) StarKind)
    go = do
      _ <- equalToken Data
      (v, _) <-
        consumeToken
          (\case
             Constructor i -> Just i
             _ -> Nothing) <?>
        "new type name (e.g. Foo)"
      vs <- many tyvar
      _ <- equalToken Equals
      cs <- sepBy1 consp (equalToken Bar)
      _ <- (pure () <* satisfyToken (== NonIndentedNewline)) <|> endOfTokens
      pure (DataType (Identifier (T.unpack v)) vs cs)

kindParser :: Stream s m (Token, Location) => ParsecT s Int m Kind
kindParser = infix'
  where
    infix' = do
      left <- star
      tok <-
        fmap Just (operator <?> ("arrow " ++ curlyQuotes "->")) <|> pure Nothing
      case tok of
        Just (RightArrow, _) -> do
          right <-
            kindParser <?>
            ("right-hand side of function arrow " ++ curlyQuotes "->")
          pure (FunctionKind left right)
        _ -> pure left
      where
        operator =
          satisfyToken
            (\case
               RightArrow {} -> True
               _ -> False)
    star = do
      (c, _) <-
        consumeToken
          (\case
             Constructor c
               | c == "Type" -> Just StarKind
             _ -> Nothing)
      pure c

consp :: TokenParser (DataTypeConstructor ParsedType Identifier)
consp = do c <- consParser
           slots <- many slot
           pure (DataTypeConstructor c slots)
  where consParser = go <?> "value constructor (e.g. Just)"
          where
            go = do
              (c, _) <-
                consumeToken
                  (\case
                     Constructor c -> Just c
                     _ -> Nothing)
              pure
                (Identifier (T.unpack c))

slot :: TokenParser (ParsedType Identifier)
slot = consParser <|> variableParser <|> appP
  where
    appP = parentheses go <?> "type application e.g. (Maybe Int)"
      where
        go = do
          f <- slot
          xs <- many1 slot
          pure (foldl ParsedTypeApp f xs)
        parentheses p = do
          _ <- equalToken OpenParen
          e <- p <?> "type inside parentheses e.g. (Maybe a)"
          _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
          pure e
    variableParser = go <?> "type variable (e.g. ‘a’, ‘s’, etc.)"
      where
        go = do
          (v, _) <-
            consumeToken
              (\case
                 Variable i -> Just i
                 _ -> Nothing)
          pure (ParsedTypeVariable (Identifier (T.unpack v)))
    consParser = go <?> "type constructor (e.g. Maybe)"
      where
        go = do
          (c, _) <-
            consumeToken
              (\case
                 Constructor c -> Just c
                 _ -> Nothing)
          pure (ParsedTypeConstructor (Identifier (T.unpack c)))

parsedType :: TokenParser (ParsedType Identifier)
parsedType = infix' <|> app <|> unambiguous
  where
    infix' = do
      left <- (app <|> unambiguous) <?> "left-hand side of function arrow"
      tok <-
        fmap Just (operator <?> ("function arrow " ++ curlyQuotes "->")) <|>
        pure Nothing
      case tok of
        Just (RightArrow, _) -> do
          right <-
            parsedType <?>
            ("right-hand side of function arrow " ++ curlyQuotes "->")
          pure
            (ParsedTypeApp
               (ParsedTypeApp (ParsedTypeConstructor (Identifier "(->)")) left)
               right)
        _ -> pure left
      where
        operator =
          satisfyToken
            (\case
               RightArrow {} -> True
               _ -> False)
    app = do
      f <- unambiguous
      args <- many unambiguous
      pure (foldl' ParsedTypeApp f args)
    unambiguous = atomicType <|> parensTy parsedType
    atomicType = consParse <|> varParse
    consParse = do
      (v, _) <-
        consumeToken
          (\case
             Constructor i -> Just i
             _ -> Nothing) <?>
        "type constructor (e.g. Int, Maybe)"
      pure
        (ParsedTypeConstructor
           (Identifier (T.unpack v)))
    varParse = do
      (v, _) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing) <?>
        "type variable (e.g. a, f)"
      pure (ParsedTypeVariable (Identifier (T.unpack v)))
    parensTy p = go <?> "parentheses e.g. (T a)"
      where
        go = do
          _ <- equalToken OpenParen
          e <- p <?> "type inside parentheses e.g. (Maybe a)"
          _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
          pure e

varfundecl :: TokenParser (ImplicitlyTypedBinding Identifier Location)
varfundecl = go <?> "variable declaration (e.g. x = 1, f = \\x -> x * x)"
  where
    go = do
      (v, loc) <-
         consumeToken
           (\case
              Variable i -> Just i
              _ -> Nothing) <?>
         "variable name"
      _ <- equalToken Equals <?> "‘=’ for variable declaration e.g. x = 1"
      e <- expParser
      _ <- (pure () <* satisfyToken (==NonIndentedNewline)) <|> endOfTokens
      pure (ImplicitlyTypedBinding loc (Identifier (T.unpack v)) [makeAlt loc e])

makeAlt :: l -> Expression i l -> Alternative i l
makeAlt loc e =
  case e of
    LambdaExpression _ alt -> alt
    _ -> Alternative loc [] e

case' :: TokenParser (Expression Identifier Location)
case' = do
  u <- getState
  loc <- equalToken Case
  setState (locationStartColumn loc)
  e <- expParser <?> "expression to do case analysis e.g. case e of ..."
  _ <-  equalToken Of
  p <- lookAhead altPat <?> "case pattern"
  alts <- many (altParser e (locationStartColumn (patternLabel p)))
  setState u
  pure (CaseExpression loc e alts)

altParser
  :: Expression Identifier Location
  -> Int
  -> TokenParser (Pattern Identifier Location, Expression Identifier Location)
altParser e' startCol =
  (do u <- getState
      p <- altPat
      when
        (locationStartColumn (patternLabel p) /= startCol)
        (unexpected
           ("pattern at column " ++
            show (locationStartColumn (patternLabel p)) ++
            ", it should start at column " ++
            show startCol ++ " to match the others"))
      setState startCol
      _ <- equalToken RightArrow
      e <- expParser
      setState u
      pure (p, e)) <?>
  "indented case alternative e.g.\n\n\
  \case " ++
  printExpression (const Nothing) e' ++
  " of\n\
  \  Just bar -> bar"

altPat :: TokenParser (Pattern Identifier Location)
altPat = varp <|> consParser
  where
    patInner = parenpat <|> varp <|> unaryConstructor
    parenpat = go
      where
        go = do
          _ <- equalToken OpenParen
          e <- varp <|> altPat
          _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
          pure e
    varp = go <?> "variable pattern (e.g. x)"
      where
        go = do
          (v, loc) <-
            consumeToken
              (\case
                 Variable i -> Just i
                 _ -> Nothing)
          pure (if T.isPrefixOf "_" v
                   then WildcardPattern loc (T.unpack v)
                   else VariablePattern loc (Identifier (T.unpack v)))
    unaryConstructor = go <?> "unary constructor (e.g. Nothing)"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Constructor c -> Just c
                 _ -> Nothing)
          pure (ConstructorPattern loc (Identifier (T.unpack c)) [])
    consParser = go <?> "constructor pattern (e.g. Just x)"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Constructor c -> Just c
                 _ -> Nothing)
          args <- many patInner
          pure (ConstructorPattern loc (Identifier (T.unpack c)) args)

expParser :: TokenParser (Expression Identifier Location)
expParser = case' <|> lambda <|> ifParser <|> infix' <|> app <|> atomic
  where
    app = do
      left <- funcOp <?> "function expression"
      right <- many unambiguous <?> "function arguments"
      case right of
        [] -> pure left
        _ -> pure (foldl (ApplicationExpression (Location 0 0 0 0)) left right)
    infix' =
      (do left <- (app <|> unambiguous) <?> "left-hand side of operator"
          tok <- fmap Just (operator <?> "infix operator") <|> pure Nothing
          case tok of
            Just (Operator t, _) -> do
              right <-
                (app <|> unambiguous) <?>
                ("right-hand side of " ++
                 curlyQuotes (T.unpack t) ++ " operator")
              badop <- fmap Just (lookAhead operator) <|> pure Nothing
              let infixexp =
                    InfixExpression
                      (Location 0 0 0 0)
                      left
                      (Identifier (T.unpack t))
                      right
              maybe
                (return ())
                (\op ->
                   unexpected
                     (concat
                        [ tokenString op ++
                          ". When more than one operator is used\n"
                        , "in the same expression, use parentheses, like this:\n"
                        , "(" ++
                          printExpression (const Nothing) infixexp ++
                          ") " ++
                          (case op of
                             (Operator i, _) -> T.unpack i ++ " ..."
                             _ -> "* ...") ++
                          "\n"
                        , "Or like this:\n"
                        , printExpressionAppArg (const Nothing) left ++
                          " " ++
                          T.unpack t ++
                          " (" ++
                          printExpressionAppArg (const Nothing) right ++
                          " " ++
                          case op of
                            (Operator i, _) -> T.unpack i ++ " ...)"
                            _ -> "* ...)"
                        ]))
                badop
              pure infixexp
            _ -> pure left) <?>
      "infix expression (e.g. x * y)"
      where
        operator =
          satisfyToken
            (\case
               Operator {} -> True
               _ -> False)
    funcOp = varParser <|> constructorParser <|> parensExpr
    unambiguous = parensExpr <|> atomic
    parensExpr = parens expParser

lambda :: TokenParser (Expression Identifier Location)
lambda = do
  loc <- equalToken Backslash <?> "lambda expression (e.g. \\x -> x)"
  args <- many1 funcParam <?> "lambda parameters"
  _ <- equalToken RightArrow
  e <- expParser
  pure (LambdaExpression loc (Alternative loc args e))

funcParam :: TokenParser (Pattern Identifier Location)
funcParam = go <?> "function parameter (e.g. ‘x’, ‘limit’, etc.)"
  where
    go = do
      (v, loc) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing)
      pure (VariablePattern loc (Identifier (T.unpack v)))

atomic :: TokenParser (Expression Identifier Location)
atomic =
  varParser <|> charParser <|> stringParser <|> integerParser <|> decimalParser <|>
  constructorParser
  where
    charParser = go <?> "character (e.g. 'a')"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Character c -> Just c
                 _ -> Nothing)
          pure (LiteralExpression loc (CharacterLiteral c))
    stringParser = go <?> "string (e.g. \"a\")"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 String c -> Just c
                 _ -> Nothing)
          pure (LiteralExpression loc (StringLiteral (T.unpack c)))

    integerParser = go <?> "integer (e.g. 42, 123)"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Integer c -> Just c
                 _ -> Nothing)
          pure (LiteralExpression loc (IntegerLiteral c))
    decimalParser = go <?> "decimal (e.g. 42, 123)"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Decimal c -> Just c
                 _ -> Nothing)
          pure (LiteralExpression loc (RationalLiteral (realToFrac c)))

constructorParser :: TokenParser (Expression Identifier Location)
constructorParser = go <?> "constructor (e.g. Just)"
  where
    go = do
      (c, loc) <-
        consumeToken
          (\case
             Constructor c -> Just c
             _ -> Nothing)
      pure
        (ConstructorExpression loc (Identifier (T.unpack c)))

parens :: TokenParser a -> TokenParser a
parens p = go <?> "parens e.g. (x)"
  where go = do
         _ <- equalToken OpenParen
         e <- p <?> "expression inside parentheses e.g. (foo)"
         _ <- equalToken CloseParen<?> "closing parenthesis ‘)’"
         pure e

varParser :: TokenParser (Expression Identifier Location)
varParser = go <?> "variable (e.g. ‘foo’, ‘id’, etc.)"
  where
    go = do
      (v, loc) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing)
      pure (if T.isPrefixOf "_" v
               then ConstantExpression loc (Identifier (T.unpack v))
               else VariableExpression loc (Identifier (T.unpack v)))

ifParser :: TokenParser (Expression Identifier Location)
ifParser = go <?> "if expression (e.g. ‘if p then x else y’)"
  where
    go = do
      loc <- equalToken If
      p <- expParser <?> "condition expresion of if-expression"
      _ <- equalToken Then <?> "‘then’ keyword for if-expression"
      e1 <- expParser <?> "‘then’ clause of if-expression"
      _ <- equalToken Else <?> "‘else’ keyword for if-expression"
      e2 <- expParser <?> "‘else’ clause of if-expression"
      pure
        (IfExpression
           loc
           { locationEndLine = locationEndLine (expressionLocation loc e2)
           , locationEndColumn = locationEndColumn (expressionLocation loc e2)
           }
           p
           e1
           e2)
    expressionLocation nil e = foldr const nil e
