{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Duet syntax tokenizer.

module Duet.Tokenizer where

import           Control.Monad
import           Data.Char
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Types
import           Text.Parsec
import           Text.Parsec.Text
import           Text.Printf

data Token
  = If
  | Then
  | Else
  | Case
  | Let
  | In
  | OpenParen
  | CloseParen
  | Equals
  | Variable !Text
  | Constructor !Text
  | Character !Char
  | String !Text
  | Operator !Text
  | Comma
  | Integer !Integer
  | Decimal !Double
  deriving (Show, Eq, Ord)

tokenize :: FilePath -> Text -> Either ParseError [(Token, Location)]
tokenize = parse tokensTokenizer

tokensTokenizer :: Parser [(Token, Location)]
tokensTokenizer = spaces *> many (tokenTokenizer <* spaces) <* eof

tokenTokenizer :: Parser (Token, Location)
tokenTokenizer =
  choice
    [ atom If "if"
    , atom Then "then"
    , atom Else "else"
    , atom Case "case"
    , atom OpenParen "("
    , atom CloseParen ")"
    , atom Equals "="
    , atom Let "let"
    , atom In "in"
    , atom Comma ","
    , parsing
        Operator
        (fmap
           T.pack
           (choice
              [ string "*"
              , string "+"
              , try (string ">=")
              , try (string "<=")
              , string ">"
              , string "<"
              , string "/"
              ]))
        "operator (e.g. *, <, +, etc.)"
    , specialParsing
        Character
        (do _ <- string "'"
            chars <- many1 (satisfy (/= '\''))
            when
              (length chars > 1)
              (unexpected
                 (concat
                    [ "character: you wrote\n"
                    , "'" ++ ellipsis 5 chars ++ "\n"
                    , "but only one character is allowed inside single quotes, like this:\n'" ++
                      take 1 chars ++ "'"
                    , "\nPerhaps you forgot to put the closing single quote?\n"
                    , "You may also have meant to use double quotes for text, e.g.\n"
                    , "\"" ++ takeWhile (/= '\'') chars ++ "\""
                    ]))
            _ <- string "'"
            pure (head chars))
        "character (e.g. 'a', 'z', '9', etc.)"
    , parsing
        String
        (do _ <- string "\""
            chars <- many1 (satisfy (\c -> c /= '"'))
            when
              (any (== '\\') chars)
              (unexpected "\\ character, not allowed inside a string.")
            _ <- string "\"" <?> "double quotes (\") to close the string"
            pure (T.pack chars))
        "string (e.g. \"hello\", \"123\", etc.)"
    , parsing
        Constructor
        (do c <- satisfy isUpper
            variable <- many (satisfy (flip elem ['a' .. 'z']))
            pure (T.singleton c <> T.pack variable))
        "constructor (e.g. “Rocket”, “Just”, etc.)"
    , parsing
        Variable
        (do variable <-
              do start <- many1 (satisfy (flip elem ("_" ++ ['a' .. 'z'])))
                 end <-
                   many
                     (satisfy (flip elem ("_" ++ ['a' .. 'z'] ++ ['0' .. '9'])))
                 pure (start ++ end)
            pure (T.pack variable))
        "variable (e.g. “elephant”, “age”, “t2”, etc.)"
    , parseNumbers
    ]

ellipsis :: Int -> [Char] -> [Char]
ellipsis n text =
  if length text > 2
    then take n text ++ "…"
    else text

specialParsing ::  (t1 -> t) -> Parser  t1 -> String -> Parser  (t, Location)
specialParsing constructor parser description = do
  start <- getPosition
  thing <- parser <?> description
  end <- getPosition
  pure
    ( constructor thing
    , Location
        (sourceLine start)
        (sourceColumn start)
        (sourceLine end)
        (sourceColumn end))

atom ::  t -> String -> Parser  (t, Location)
atom constructor text = do
  start <- getPosition
  _ <- try (string text) <?> smartQuotes text
  end <- getPosition
  pure
    ( constructor
    , Location
        (sourceLine start)
        (sourceColumn start)
        (sourceLine end)
        (sourceColumn end))

parsing ::  (Text -> t) -> Parser  Text -> String -> Parser  (t, Location)
parsing constructor parser description = do
  start <- getPosition
  text <- parser <?> description
  mapM_
    (bailOnUnsupportedKeywords text)
    [ "class"
    , "data"
    , "default"
    , "deriving"
    , "do"
    , "forall"
    , "import"
    , "infix"
    , "infixl"
    , "infixr"
    , "instance"
    , "module"
    , "newtype"
    , "qualified"
    , "type"
    , "where"
    , "foreign"
    , "ccall"
    , "as"
    , "safe"
    , "unsafe"
    ]
  end <- getPosition
  pure
    ( constructor text
    , Location
        (sourceLine start)
        (sourceColumn start)
        (sourceLine end)
        (sourceColumn end))
  where
    bailOnUnsupportedKeywords text word =
      when
        (text == word)
        (unexpected
           ("“" ++ T.unpack word ++ "”: that keyword isn't allowed, " ++ ext))
      where
        ext = "but you could use this instead: " ++ T.unpack word ++ "_"

parseNumbers :: Parser (Token, Location)
parseNumbers = parser <?> "number (e.g. 42, 3.141, etc.)"
  where
    parser = do
      start <- getPosition
      neg <- fmap Just (char '-') <|> pure Nothing
      let operator = do
            end <- getPosition
            pure
              ( Operator "-"
              , Location
                  (sourceLine start)
                  (sourceColumn start)
                  (sourceLine end)
                  (sourceColumn end))
          number :: (forall a. (Num a) => a -> a) -> Parser (Token, Location)
          number f = do
            x <- many1 digit
            (do _ <- char '.'
                y <- many1 digit <?> ("decimal component, e.g. " ++ x ++ ".0")
                end <- getPosition
                pure
                  ( Decimal (f (read (x ++ "." ++ y)))
                  , Location
                      (sourceLine start)
                      (sourceColumn start)
                      (sourceLine end)
                      (sourceColumn end))) <|>
              (do end <- getPosition
                  pure
                    ( Integer (f (read x))
                    , Location
                        (sourceLine start)
                        (sourceColumn start)
                        (sourceLine end)
                        (sourceColumn end)))
      case neg of
        Nothing -> number id
        Just {} -> number (* (-1)) <|> operator

smartQuotes :: [Char] -> [Char]
smartQuotes t = "“" <> t <> "”"

curlyQuotes :: [Char] -> [Char]
curlyQuotes t = "‘" <> t <> "’"

type TokenParser = Parsec (Token, Location) ()

-- | Consume the given predicate from the token stream.
consumeToken :: Stream s m (Token, Location) => (Token -> Maybe a) -> ParsecT s u m (a, Location)
consumeToken f =
  tokenPrim tokenString
            tokenPosition
            (\(x,tok) -> fmap (, tok) (f x))

-- | Consume the given predicate from the token stream.
satisfyToken :: Stream s m (Token, Location) => (Token -> Bool) -> ParsecT s u m (Token, Location)
satisfyToken f =
  tokenPrim tokenString
            tokenPosition
            (\(tok, l) -> if f tok
                             then Just (tok, l)
                             else Nothing)

-- | The parser @anyToken@ accepts any kind of token. It is for example
-- used to implement 'eof'. Returns the accepted token.
anyToken :: (Stream s m (Token, Location)) => ParsecT s u m (Token, Location)
anyToken = consumeToken Just

-- | Make a string out of the token, for error message purposes.
tokenString :: (Token, Location) -> [Char]
tokenString (tok, _) =
  case tok of
    If -> curlyQuotes "if"
    Then -> curlyQuotes "then"
    Else -> curlyQuotes "else"
    Case -> curlyQuotes "case"
    Let -> curlyQuotes "let"
    In -> curlyQuotes "in"
    OpenParen -> "open parenthesis " ++ curlyQuotes "("
    CloseParen -> "close parenthesis " ++ curlyQuotes ")"
    Equals -> curlyQuotes "="
    Variable t -> "variable " ++ curlyQuotes (T.unpack t)
    Constructor t -> "constructor " ++ curlyQuotes (T.unpack t)
    Character !c -> "character " ++ curlyQuotes (T.unpack (T.singleton c))
    String !t -> "string " ++ show t
    Operator !t -> "operator " ++ curlyQuotes (T.unpack t)
    Comma -> curlyQuotes ","
    Integer !i -> "integer " ++ show i
    Decimal !d -> "decimal " ++ printf "%f" d

-- | Update the position by the token.
tokenPosition :: SourcePos -> (Token, Location) -> t -> SourcePos
tokenPosition pos (_, l) _ =
  setSourceColumn (setSourceLine pos line) col
  where (line,col) = (locationStartLine l, locationStartColumn l)
