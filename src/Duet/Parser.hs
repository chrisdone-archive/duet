-- |

module Duet.Parser where

import Data.Char
import Duet.Types
import Text.Parsec
import Text.Parsec.Text

implicitlyTypedBindingParser :: Parser (ImplicitlyTypedBinding Location)
implicitlyTypedBindingParser = do
  identifier <- identifierParser
  alternative <- alternativeParser
  pure (ImplicitlyTypedBinding Location identifier [alternative])

identifierParser :: Parser Identifier
identifierParser = fmap Identifier (many1 (satisfy isAlphaNum))

alternativeParser :: Parser (Alternative Location)
alternativeParser = do
  patterns <- many (spaces*>patternParser<*spaces)
  _ <- string "="
  spaces
  expression <- expressionParser
  pure (Alternative Location patterns expression)

patternParser :: Parser Pattern
patternParser = variableParser
  where variableParser = VariablePattern <$> identifierParser

expressionParser :: Parser (Expression Location)
expressionParser = integralParser <|> applicationParser <|> variableParser
  where
    integralParser = (LiteralExpression Location . IntegerLiteral . read) <$> many1 digit
    applicationParser =
      string "(" *>
      (ApplicationExpression Location <$> variableParser <*> (spaces *> expressionParser) <*
       string ")")
    variableParser = VariableExpression Location <$> identifierParser
