{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Test suite.

module Main (main) where

import qualified Data.Text as T
import           Duet.IDE
import           Duet.IDE.Spec
import           Duet.IDE.Test
import           Duet.IDE.Types (State(..))
import           Duet.IDE.View
import           React.Flux ((@=))
import qualified React.Flux as Flux

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  Flux.reactRender
    "app"
    (Flux.defineControllerView
       "State"
       store
       (\state () -> testview state (renderModule (stateCursor state) (stateAST state))))
    ()
  runTest tests

----------------------------------------------------------------------
-- View

testview ::
     State
  -> Flux.ReactElementM Flux.ViewEventHandler ()
  -> Flux.ReactElementM Flux.ViewEventHandler ()
testview state view = do
  Flux.h1_ ["key" @= "title"] (Flux.elemText "Test suite")
  Flux.pre_ ["key" @= "current-state"] (Flux.elemText (T.pack (show state)))
  view
