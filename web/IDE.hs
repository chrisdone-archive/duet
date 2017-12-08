{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

-- | IDE itself.

module Main where

import           Control.Monad
import           Duet.IDE
import           Duet.IDE.Types
import           Duet.IDE.View
import qualified React.Flux as Flux
import qualified React.Flux.Events as Flux.Events
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  mstate <- Flux.Persist.getAppStateVal
  maybe (return ()) (Flux.alterStore store . ReplaceState) mstate
  Flux.Events.onBodyKeydown
    (\shift key ->
       case key of
         8 -> Flux.alterStore store (KeyDown shift BackspaceKey)
         9 -> Flux.alterStore store (KeyDown shift TabKey)
         37 -> Flux.alterStore store (KeyDown shift LeftKey)
         39 -> Flux.alterStore store (KeyDown shift RightKey)
         38 -> Flux.alterStore store (KeyDown shift UpKey)
         40 -> Flux.alterStore store (KeyDown shift DownKey)
         13 -> Flux.alterStore store (KeyDown shift ReturnKey)
         _ -> when False (print ("keydown", key)))
  Flux.Events.onBodyKeypress
    (\key -> do
       when False (print ("keypress", key))
       Flux.alterStore store (KeyPress key))
  Flux.reactRender
    "app"
    (Flux.defineControllerView
       "State"
       store
       (\state () -> renderModule (stateCursor state) (stateAST state)))
    ()
