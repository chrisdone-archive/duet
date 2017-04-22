{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Parser where

import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Tokenizer
import           Duet.Types
import           Text.Parsec hiding (satisfy)

parseText :: SourceName -> Text -> Either ParseError (Expression Location)
parseText fp inp =
  case parse tokensTokenizer fp inp of
    Left e -> Left e
    Right tokens ->
      case parse tokensParser fp tokens of
        Left e -> Left e
        Right ast -> Right ast

tokensParser :: Stream s m (Token, Location) => ParsecT s u m (Expression Location)
tokensParser = expParser

expParser :: Stream s m (Token, Location) => ParsecT s u m (Expression Location)
expParser = do
  ifParser <|> varParser

varParser :: Stream s m (Token, Location) => ParsecT s u m (Expression Location)
varParser = do
  (v, loc) <-
    consumeToken
      (\case
         Variable i -> Just i)
  pure (VariableExpression loc (Identifier (T.unpack v)))

ifParser :: Stream s m (Token, Location) => ParsecT s u m (Expression Location)
ifParser = do
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
  where
    expressionLocation nil e = foldr const nil e
