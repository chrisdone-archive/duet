{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Parser where

import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Tokenizer
import           Duet.Types
import           Text.Parsec hiding (satisfy)

parseText :: SourceName -> Text -> Either ParseError (Expression Location)
parseText fp inp =
  case parse tokensTokenizer fp inp of
    Left e -> Left e
    Right tokens' ->
      case parse tokensParser fp tokens' of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: TokenParser (Expression Location)
tokensParser = expParser

expParser :: TokenParser (Expression Location)
expParser = do
  varParser <|> ifParser

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
      p <- expParser
      _ <- satisfyToken (== Then)
      e1 <- expParser
      _ <- satisfyToken (== Else)
      e2 <- expParser
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
