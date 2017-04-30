{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Parser where

import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Printer
import           Duet.Tokenizer
import           Duet.Types
import           Text.Parsec hiding (satisfy, anyToken)

parseText :: SourceName -> Text -> Either ParseError [Decl FieldType Identifier Location]
parseText fp inp =
  case parse tokensTokenizer fp (inp) of
    Left e -> Left e
    Right tokens' ->
      case parse tokensParser fp tokens' of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: TokenParser [Decl FieldType Identifier Location]
tokensParser = moduleParser <* endOfTokens

moduleParser :: TokenParser [Decl FieldType Identifier Location]
moduleParser =
  many
    ((fmap (\x -> BindGroupDecl (BindGroup [] [[x]])) varfundecl) <|>
     fmap DataDecl datadecl)

datadecl :: TokenParser (DataType FieldType Identifier)
datadecl = go <?> "data declaration (e.g. data Maybe a = Just a | Nothing)"
  where
    tyvar = do
      (v, loc) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing) <?>
        "variable name"
      pure (TypeVariable (Identifier (T.unpack v)) StarKind)
    go = do
      _ <- equalToken Data
      (v, loc) <-
        consumeToken
          (\case
             Constructor i -> Just i
             _ -> Nothing) <?>
        "new type name (e.g. Foo)"
      vs <- many tyvar
      _ <- equalToken Equals
      cs <- sepBy1 consp (equalToken Bar)
      _ <- (pure () <* satisfyToken (==NonIndentedNewline)) <|> eof
      pure (DataType (Identifier (T.unpack v)) vs cs)

consp :: TokenParser (DataTypeConstructor FieldType Identifier)
consp = do c <- constructorParser
           slots <- many slot
           pure (DataTypeConstructor c slots)
  where constructorParser = go <?> "value constructor (e.g. Just)"
          where
            go = do
              (c, loc) <-
                consumeToken
                  (\case
                     Constructor c -> Just c
                     _ -> Nothing)
              pure
                (Identifier (T.unpack c))

slot :: TokenParser (FieldType Identifier)
slot = constructorParser <|> varParser <|> appP
  where
    appP = parens go <?> "type application e.g. (Maybe Int)"
      where
        go = do
          f <- slot
          xs <- many1 slot
          pure (foldl FieldTypeApp f xs)
        parens p = do
          _ <- equalToken OpenParen
          e <- p <?> "type inside parentheses e.g. (Maybe a)"
          _ <- equalToken CloseParen <?> "closing parenthesis ‘)’"
          pure e
    varParser = go <?> "type variable (e.g. ‘a’, ‘s’, etc.)"
      where
        go = do
          (v, loc) <-
            consumeToken
              (\case
                 Variable i -> Just i
                 _ -> Nothing)
          pure (FieldTypeVariable (Identifier (T.unpack v)))
    constructorParser = go <?> "type constructor (e.g. Maybe)"
      where
        go = do
          (c, loc) <-
            consumeToken
              (\case
                 Constructor c -> Just c
                 _ -> Nothing)
          pure (FieldTypeConstructor (Identifier (T.unpack c)))

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
      _ <- equalToken Equals
      e <- expParser
      _ <- (pure () <* satisfyToken (==NonIndentedNewline)) <|> eof
      pure (ImplicitlyTypedBinding loc (Identifier (T.unpack v)) [Alternative loc [] e])

expParser :: TokenParser (Expression Identifier Location)
expParser = lambda <|> ifParser <|> infix' <|> app <|> atomic
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
  _ <- equalToken Arrow
  e <- expParser
  pure (LambdaExpression loc (Alternative loc args e))

funcParam :: TokenParser (Pattern Identifier)
funcParam = go <?> "function parameter (e.g. ‘x’, ‘limit’, etc.)"
  where
    go = do
      (v, _) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing)
      pure (VariablePattern (Identifier (T.unpack v)))

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
      pure (VariableExpression loc (Identifier (T.unpack v)))

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
