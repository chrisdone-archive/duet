{-# LANGUAGE OverloadedStrings #-}

module Main where

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
         57 -> Flux.alterStore store (KeyDown shift OpenParens)
         48 -> Flux.alterStore store (KeyDown shift CloseParens)
         13 -> Flux.alterStore store (KeyDown shift ReturnKey)
         _ -> print key)
  Flux.Events.onBodyKeypress (Flux.alterStore store . KeyPress)
  Flux.reactRender "app" (Flux.defineControllerView "State" store appview) ()
