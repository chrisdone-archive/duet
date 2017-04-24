{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Parser where

import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Printer
import           Duet.Tokenizer
import           Duet.Types
import           Text.Parsec hiding (satisfy, anyToken)

parseText :: SourceName -> Text -> Either ParseError [BindGroup Location]
parseText fp inp =
  case parse tokensTokenizer fp (inp) of
    Left e -> Left e
    Right tokens' ->
      case parse tokensParser fp tokens' of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: TokenParser [BindGroup Location]
tokensParser = moduleParser <* endOfTokens

moduleParser :: TokenParser [BindGroup Location]
moduleParser = fmap (map (\x -> BindGroup [] [[x]])) (many varfundecl)

varfundecl :: TokenParser (ImplicitlyTypedBinding Location)
varfundecl = go <?> "variable declaration (e.g. x = 1, f = \\x -> x * x)"
  where
    go = do
      (v, loc) <-
         consumeToken
           (\case
              Variable i -> Just i
              _ -> Nothing) <?>
         "variable name"
      _ <- satisfyToken (== Equals)
      e <- expParser
      _ <- (pure () <* satisfyToken (==NonIndentedNewline)) <|> eof
      pure (ImplicitlyTypedBinding loc (Identifier (T.unpack v)) [Alternative loc [] e])

expParser :: TokenParser (Expression Location)
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
            Nothing -> pure left
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
                          printExpression infixexp ++
                          ") " ++
                          (case op of
                             (Operator i, _) -> T.unpack i ++ " ..."
                             _ -> "* ...") ++
                          "\n"
                        , "Or like this:\n"
                        , printExpressionAppArg left ++
                          " " ++
                          T.unpack t ++
                          " (" ++
                          printExpressionAppArg right ++
                          " " ++
                          case op of
                            (Operator i, _) -> T.unpack i ++ " ...)"
                            _ -> "* ...)"
                        ]))
                badop
              pure infixexp) <?>
      "infix expression (e.g. x * y)"
      where
        operator =
          satisfyToken
            (\case
               Operator {} -> True
               _ -> False)
    funcOp = varParser <|> parensExpr
    unambiguous = parensExpr <|> atomic
    parensExpr = parens expParser

lambda :: TokenParser (Expression Location)
lambda = do
  (_, loc) <-
    satisfyToken (== Backslash) <?> "lambda expression (e.g. \\x -> x)"
  args <- many1 funcParam <?> "lambda parameters"
  _ <- satisfyToken (== Arrow)
  e <- expParser
  pure
    (LambdaExpression
       loc
       (Alternative loc args e))

funcParam :: TokenParser Pattern
funcParam = go <?> "function parameter (e.g. ‘x’, ‘limit’, etc.)"
  where
    go = do
      (v, loc) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing)
      pure (VariablePattern (Identifier (T.unpack v)))

atomic :: TokenParser (Expression Location)
atomic = varParser <|> charParser <|> stringParser <|> integerParser <|> decimalParser
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

parens :: TokenParser a -> TokenParser a
parens p = go <?> "parens e.g. (x)"
  where go = do
         _ <- satisfyToken (== OpenParen)
         e <- p <?> "expression inside parentheses e.g. (foo)"
         _ <- satisfyToken (== CloseParen)<?> "closing parenthesis ‘)’"
         pure e

varParser :: TokenParser (Expression Location)
varParser = go <?> "variable (e.g. ‘foo’, ‘id’, etc.)"
  where
    go = do
      (v, loc) <-
        consumeToken
          (\case
             Variable i -> Just i
             _ -> Nothing)
      pure (VariableExpression loc (Identifier (T.unpack v)))

ifParser :: TokenParser (Expression Location)
ifParser = go <?> "if expression (e.g. ‘if p then x else y’)"
  where
    go = do
      (_, loc) <- satisfyToken (== If)
      p <- expParser <?> "condition expresion of if-expression"
      _ <- satisfyToken (== Then) <?> "‘then’ keyword for if-expression"
      e1 <- expParser <?> "‘then’ clause of if-expression"
      _ <- satisfyToken (== Else)<?> "‘else’ keyword for if-expression"
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
