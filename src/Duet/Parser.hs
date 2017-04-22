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

parseText :: SourceName -> Text -> Either ParseError (Expression Location)
parseText fp inp =
  case parse tokensTokenizer fp (inp) of
    Left e -> Left e
    Right tokens' ->
      case parse tokensParser fp tokens' of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: TokenParser (Expression Location)
tokensParser = expParser <* endOfTokens

expParser :: TokenParser (Expression Location)
expParser = ifParser <|> infix' <|> app <|> atomic
  where
    app = do
      left <- unambiguous <?> "function expression"
      right <- many unambiguous <?> "function arguments"
      case right of
        [] -> pure left
        _ -> pure (foldl (ApplicationExpression (Location 0 0 0 0)) left right)
    infix' = (do
       left <- (app <|> unambiguous) <?> "left-hand side of operator"
       tok <-
         fmap
           Just
           ((satisfyToken
              (\case
                 Operator {} -> True
                 _ -> False)) <?> "infix operator") <|>
         pure Nothing
       case tok of
         Nothing -> pure left
         Just (Operator t, _) -> do
           right <-
             (app <|> unambiguous) <?>
             ("right-hand side of " ++ curlyQuotes (T.unpack t) ++ " operator")
           pure
             (InfixExpression
                (Location 0 0 0 0)
                (VariableExpression (Location 0 0 0 0) (Identifier "op"))
                left
                right)) <?> "infix expression (e.g. x * y)"
    unambiguous = parensExpr <|> atomic
    parensExpr = parens expParser

atomic :: TokenParser (Expression Location)
atomic = varParser <|> charParser
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
