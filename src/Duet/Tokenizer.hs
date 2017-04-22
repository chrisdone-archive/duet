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

data Token
  = ImportToken
  | If
  | Then
  | Else
  | Case
  | OpenParen
  | CloseParen
  | Equals
  | Variable !Text
  | Constructor !Text
  deriving (Show, Eq, Ord)

tokenize :: FilePath -> Text -> Either ParseError [(Token, Location)]
tokenize = parse tokensTokenizer

tokensTokenizer :: Parser [(Token, Location)]
tokensTokenizer = many (spaces *> tokenTokenizer <* spaces) <* eof

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
    , parsing
        Constructor
        (do variable <- many1 digit
            pure (T.pack variable))
        "number (e.g. 0, 42, 666, 3.141, etc.)"
    ]
  where
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
          where ext = "but you could use this instead: " ++ T.unpack word ++ "'"
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

smartQuotes :: [Char] -> [Char]
smartQuotes t = "“" <> t <> "”"
