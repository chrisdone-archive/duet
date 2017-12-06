{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main (main) where

import           Data.Aeson
import           Data.Monoid
import qualified Data.Text as T
import           Duet.IDE
import           Duet.IDE.Types (State(..), Keydown (..), Action(..), Cursor(..), Label(..))
import           Duet.IDE.View
import           Duet.Types
import           React.Flux.Persist (resetUUID, UUID(..))
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
       (\state () -> testview state (appview state ())))
    ()
  runTest tests

----------------------------------------------------------------------
-- Test suite

tests :: Test
tests =
  Group
    "Value declarations"
    [ Group
        "LHS"
        [ Test "Backspace no-op" typeBackspace initState
        , Test
            "Type valid name"
            (typeChars "foo")
            (makeState "foo" initExpression)
        , Test
            "Ignore invalid name"
            (typeChars "123")
            initState
        , Test
            "Type valid name, delete characters"
            (typeChars "foo" <> typeBackspace <> typeBackspace)
            (makeState "f" initExpression)
        ]
    , switchToRHS
        (Group
           "RHS"
           [ Test "Tab to RHS" [] (rhsSelectedState initExpression)
           , Test
               "Backspace no-op"
               typeBackspace
               (rhsSelectedState initExpression)
           , Group
               "Variables"
               [ Test
                   "Type variable name"
                   (typeChars "foo")
                   (rhsSelectedState
                      (VariableExpression
                         (Label {labelUUID = starterExprUUID})
                         (Identifier {identifierString = "foo"})))
               , Test
                   "Type variable name, delete characters"
                   (typeChars "foo" <> typeBackspace <> typeBackspace)
                   (rhsSelectedState
                      (VariableExpression
                         (Label {labelUUID = starterExprUUID})
                         (Identifier {identifierString = "f"})))
               , Test
                   "Type variable name, empty back to constant"
                   (typeChars "foo" <> typeBackspace <> typeBackspace <>
                    typeBackspace)
                   (rhsSelectedState
                      (ConstantExpression
                         (Label {labelUUID = starterExprUUID})
                         (Identifier {identifierString = "_"})))
               , Test
                   "Type variable name with digits"
                   (typeChars "foo123")
                   (rhsSelectedState
                      (VariableExpression
                         (Label {labelUUID = starterExprUUID})
                         (Identifier {identifierString = "foo123"})))
               ]
           , Group
               "Literals"
               [ Test
                   "Type integer literal"
                   (typeChars "123")
                   (rhsSelectedState
                      (LiteralExpression
                         (Label {labelUUID = starterExprUUID})
                         (IntegerLiteral 123)))
               , Test
                   "Type integer literal, invalid chars ignored"
                   (typeChars "123abc")
                   (rhsSelectedState
                      (LiteralExpression
                         (Label {labelUUID = starterExprUUID})
                         (IntegerLiteral 123)))
               ]
           , Group
               "Function application"
               [ Test
                   "Type space to get function application"
                   (typeChars "f ")
                   (focus
                      (UUID "1")
                      (rhsSelectedState
                         (ApplicationExpression
                            (Label {labelUUID = UUID "2"})
                            (VariableExpression
                               (Label {labelUUID = starterExprUUID})
                               (Identifier {identifierString = "f"}))
                            (ConstantExpression
                               (Label {labelUUID = UUID "1"})
                               (Identifier {identifierString = "_"})))))
               , Test
                   "Type space to get function application (2 args)"
                   (typeChars "f x y")
                   (focus
                      (UUID "5")
                      (rhsSelectedState
                         (ApplicationExpression
                            (Label {labelUUID = UUID "6"})
                            (ApplicationExpression
                               (Label {labelUUID = UUID "2"})
                               (VariableExpression
                                  (Label {labelUUID = starterExprUUID})
                                  (Identifier {identifierString = "f"}))
                               (VariableExpression
                                  (Label {labelUUID = UUID "1"})
                                  (Identifier {identifierString = "x"})))
                            (VariableExpression
                               (Label {labelUUID = UUID "5"})
                               (Identifier {identifierString = "y"})))))
               , Test
                   "Type variable argument"
                   (typeChars "f x")
                   (focus
                      (UUID "1")
                      (rhsSelectedState
                         (ApplicationExpression
                            (Label {labelUUID = UUID "2"})
                            (VariableExpression
                               (Label {labelUUID = starterExprUUID})
                               (Identifier {identifierString = "f"}))
                            (VariableExpression
                               (Label {labelUUID = UUID "1"})
                               (Identifier {identifierString = "x"})))))
               ]
           ])
    ]
  where
    rhsSelectedState e =
      (makeState initName e)
      {stateCursor = Cursor {cursorUUID = starterExprUUID}}
    focus uuid state = state {stateCursor = Cursor {cursorUUID = uuid}}
    switchToRHS =
      \case
        Test name actions state ->
          Test name (KeyDownAction False TabKey : actions) state
        Group name ts -> Group name (map switchToRHS ts)

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

----------------------------------------------------------------------
-- Test suite library

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

interactionToAction :: Interaction -> Action
interactionToAction =
  \case
    KeyDownAction s k -> KeyDown s k
    KeyPressAction k -> KeyPress (fromEnum k)

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
