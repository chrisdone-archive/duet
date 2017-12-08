{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Small test suite library.

module Duet.IDE.Test where

import           Data.Aeson (encode)
import           Duet.IDE
import           Duet.IDE.Types
import           Duet.Types
import qualified React.Flux as Flux
import           React.Flux.Persist

data Test
  = Group String [Test]
  | Test String [Interaction] State
  deriving (Show)

data Interaction
  = KeyDownAction Bool Keydown
  | KeyPressAction Char
  deriving (Show)

typeChars :: [Char] -> [Interaction]
typeChars = map KeyPressAction

typeBackspace :: [Interaction]
typeBackspace = [KeyDownAction False BackspaceKey]

typeLeft :: [Interaction]
typeLeft = [KeyDownAction False LeftKey]

typeRight :: [Interaction]
typeRight = [KeyDownAction False RightKey]

interactionToAction :: Interaction -> Action
interactionToAction =
  \case
    KeyDownAction s k -> KeyDown s k
    KeyPressAction k -> KeyPress (fromEnum k)

rhsSelectedState :: Expression Ignore Identifier Label -> State
rhsSelectedState e =
  (makeState initName e)
  {stateCursor = Cursor {cursorUUID = starterExprUUID}}

focus :: UUID -> State -> State
focus uuid state = state {stateCursor = Cursor {cursorUUID = uuid}}

switchToRHS :: Test -> Test
switchToRHS =
  \case
    Test name actions state ->
      Test name (KeyDownAction False TabKey : actions) state
    Group name ts -> Group name (map switchToRHS ts)

runTest :: Test -> IO ()
runTest t0 = do
  putStrLn "Running tests..."
  go 2 t0
  putStrLn "OK."
  where
    go indent =
      \case
        Group t ts -> do
          putStrLn (replicate indent ' ' ++ t)
          mapM_ (go (indent + 2)) ts
        Test name actions expectedState -> do
          putStrLn (replicate indent ' ' ++ name)
          Flux.alterStore store (ReplaceState initState)
          resetUUID
          mapM_ (Flux.alterStore store . interactionToAction) actions
          actualState <- Flux.getStoreData store
          if encode actualState == encode expectedState
            then pure ()
            else error
                   (unlines
                      [ "Test failed!"
                      , "Expected state: " ++ show expectedState
                      , "Actual state: " ++ show actualState
                      ])
