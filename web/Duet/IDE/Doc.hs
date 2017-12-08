{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- |

module Duet.IDE.Doc where

import           Control.Monad
import qualified Data.Text as T
import           Duet.IDE.Test
import           Duet.IDE.Types
import qualified React.Flux as Flux

renderDoc :: Test -> Flux.ReactElementM Flux.ViewEventHandler ()
renderDoc t0 = go 2 t0
  where
    go indent =
      \case
        Group t ts -> do
          heading indent t
          mapM_ (go (indent + 2)) ts
        Test name actions _ -> do
          Flux.p_ (Flux.elemText (T.pack name))
          Flux.p_ (do Flux.span_ ["className" Flux.@= "duet-key-legend"] "⌨"
                      mapM_ renderAction actions)

renderAction :: Interaction -> Flux.ReactElementM Flux.ViewEventHandler ()
renderAction = go
  where
    go =
      \case
        KeyDownAction shift keydown -> do
          when shift (key '⇧' " (shift) + ")
          case keydown of
            BackspaceKey -> key '⌫' " (backspace)"
            TabKey -> key '⇥' " (tab)"
            DownKey -> key '↓' " (down)"
            UpKey -> key '↑' " (up)"
            LeftKey -> key '←' " (left)"
            RightKey -> key '→' " (right)"
            ReturnKey -> key '⏎' " (enter/return)"
        KeyPressAction c -> key c (T.pack [c])
    key c title =
      Flux.code_
        ["className" Flux.@= "duet-key-press", "title" Flux.@= title]
        (Flux.elemText (T.pack [c]))

heading :: Int -> String -> Flux.ReactElementM Flux.ViewEventHandler ()
heading n t =
  case n of
    1 -> Flux.h1_ [] (Flux.elemText (T.pack t))
    2 -> Flux.h2_ [] (Flux.elemText (T.pack t))
    3 -> Flux.h3_ [] (Flux.elemText (T.pack t))
    _ -> Flux.h4_ [] (Flux.elemText (T.pack t))
