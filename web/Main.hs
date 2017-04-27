{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Duet.Parser as Duet
import qualified Duet.Types as Duet
import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  source <-
    Snappy.textbox
      snap
      (pure 20)
      (pure 20)
      (pure 500)
      (pure 250)
      (pure defaultSource)
  let result =
        fmap
          (Duet.parseText "<input box>" . T.pack)
          (Snappy.eventToDynamic defaultSource (Snappy.textboxChange source))
  Snappy.textbox
    snap
    (pure 20)
    (pure 290)
    (pure 500)
    (pure 250)
    (fmap
       (\case
          Left err -> show err
          Right ast -> show ast)
       result)
  pure ()

defaultSource :: String
defaultSource =
  unlines
    [ "compose = \\f g x -> f (g x)"
    , "id = \\x -> x"
    , "demo = id (if id True"
    , "              then \"Yay!\""
    , "              else id id \"Nay!\")"
    ]
