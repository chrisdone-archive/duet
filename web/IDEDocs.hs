{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | IDE documentation.

module Main (main) where

import           Duet.IDE.Doc
import           Duet.IDE.Spec
import           React.Flux ((@=))
import qualified React.Flux as Flux

--------------------------------------------------------------------------------
-- Main entry point

data State = State
data Action = Action

instance Flux.StoreData State where
  type StoreAction State = Action
  transform Action State = pure State

main :: IO ()
main = do
  Flux.reactRender
    "app"
    (Flux.defineControllerView
       "State"
       (Flux.mkStore State)
       (\State () -> testview (mapM_ renderDoc tests)))
    ()

----------------------------------------------------------------------
-- View

testview ::
     Flux.ReactElementM Flux.ViewEventHandler ()
  -> Flux.ReactElementM Flux.ViewEventHandler ()
testview view = do
  Flux.h1_ ["key" @= "title"] (Flux.elemText "Editor functionality")
  view
