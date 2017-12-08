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
import           Duet.IDE.Types (Ignore, State(..), Keydown (..), Action(..), Cursor(..), Label(..))
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
       (\state () -> testview state (renderModule (stateCursor state) (stateAST state))))
    ()
  runTest tests

----------------------------------------------------------------------
-- Test suite

tests :: Test
tests =
  Group
    "Value declarations"
    [Group "LHS" lhsTests, switchToRHS (Group "RHS" rhsTests)]

lhsTests :: [Test]
lhsTests =
  [ Test "Backspace no-op" typeBackspace initState
  , Test "Type valid name" (typeChars "foo") (makeState "foo" initExpression)
  , Test "Ignore invalid name (digits)" (typeChars "123") initState
  , Test "Ignore invalid name (upper)" (typeChars "F") initState
  , Test
      "Type valid name, delete characters"
      (typeChars "foo" <> typeBackspace <> typeBackspace)
      (makeState "f" initExpression)
  ]

rhsTests :: [Test]
rhsTests =
  [ Test "Tab to RHS" [] (rhsSelectedState initExpression)
  , Test "Backspace no-op" typeBackspace (rhsSelectedState initExpression)
  , Test
      "Parens"
      (typeChars "(")
      (focus
         (UUID "2")
         (rhsSelectedState
            (ParensExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"})))))
  , Group "If" ifTests
  , Group "Lambda" lambdaTests
  , Group
      "Case"
      caseTests
  , Group "Variables" variableTests
  , Group
      "Literals"
      literalTests
  , Group "Function application" functionApplicationTests
  , Group "Infix" infixTests
  ]

literalTests :: [Test]
literalTests =
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

caseTests :: [Test]
caseTests =
  [ Test
      "Case completion"
      (typeChars "case ")
      (focus
         (UUID "2")
         (rhsSelectedState
            (CaseExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"}))
               [ CaseAlt
                 { caseAltLabel = Label {labelUUID = UUID "3"}
                 , caseAltPattern =
                     WildcardPattern (Label {labelUUID = UUID "4"}) "_"
                 , caseAltExpression =
                     ConstantExpression
                       (Label {labelUUID = UUID "5"})
                       (Identifier {identifierString = "_"})
                 }
               ])))
  ]

lambdaTests :: [Test]
lambdaTests =
  [ Test
      "Lambda completion"
      (typeChars "\\")
      (focus
         (UUID "3")
         (rhsSelectedState
            (LambdaExpression
               (Label {labelUUID = UUID "1"})
               (Alternative
                { alternativeLabel = Label {labelUUID = UUID "2"}
                , alternativePatterns =
                    [WildcardPattern (Label {labelUUID = UUID "3"}) "_"]
                , alternativeExpression =
                    ConstantExpression
                      (Label {labelUUID = UUID "4"})
                      (Identifier {identifierString = "_"})
                }))))
  ]

ifTests :: [Test]
ifTests =
  [ Test
      "If completion"
      (typeChars "if ")
      (focus
         (UUID "2")
         (rhsSelectedState
            (IfExpression
               (Label {labelUUID = UUID "1"})
               (ConstantExpression
                  (Label {labelUUID = UUID "2"})
                  (Identifier {identifierString = "_"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "3"})
                  (Identifier {identifierString = "_"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "4"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "If deletion"
      (typeChars "if " <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  , Test
      "If deletion after then"
      (typeChars "if " <> typeRight <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  , Test
      "If deletion after else"
      (typeChars "if " <> typeRight <> typeRight <> typeBackspace)
      (focus
         (UUID "5")
         (rhsSelectedState
            (ConstantExpression
               (Label {labelUUID = UUID "5"})
               (Identifier {identifierString = "_"}))))
  ]

functionApplicationTests :: [Test]
functionApplicationTests =
  [ Test
      "Function completion"
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
      "Function completion (2 args)"
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
  , Test
      "Delete argument"
      (typeChars "f x" <> typeBackspace <> typeBackspace)
      (focus
         starterExprUUID
         (rhsSelectedState
            (VariableExpression
               (Label {labelUUID = starterExprUUID})
               (Identifier {identifierString = "f"}))))
  , Test
      "Delete function"
      (typeChars "f x" <> typeLeft <> typeBackspace <> typeBackspace)
      (focus
         (UUID "1")
         (rhsSelectedState
            (VariableExpression
               (Label {labelUUID = UUID "1"})
               (Identifier {identifierString = "x"}))))
  , Test
      "Add argument inbetween func and arg"
      (typeChars "f x" <> typeLeft <> typeChars " ")
      (focus
         (UUID "3")
         (rhsSelectedState
            (ApplicationExpression
               (Label {labelUUID = UUID "2"})
               (ApplicationExpression
                  (Label {labelUUID = UUID "4"})
                  (VariableExpression
                     (Label {labelUUID = starterExprUUID})
                     (Identifier {identifierString = "f"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "3"})
                     (Identifier {identifierString = "_"})))
               (VariableExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "x"})))))
  ]

infixTests :: [Test]
infixTests =
  [ Test
      "Infix completion (after hole)"
      (typeChars "*")
      (focus
         (UUID "1")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (ConstantExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "_"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after name)"
      (typeChars "x*")
      (focus
         (UUID "1")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "1"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after function application of hole)"
      (typeChars "x *")
      (focus
         (UUID "3")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "4"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "5"})
                   (Identifier {identifierString = "*"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "3"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix completion (after nested function application of hole)"
      (typeChars "x*y *")
      (focus
         (UUID "6")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "2"})
               (VariableExpression
                  (Label {labelUUID = UUID "STARTER-EXPR"})
                  (Identifier {identifierString = "x"}))
               ( "*"
               , VariableExpression
                   (Label {labelUUID = UUID "3"})
                   (Identifier {identifierString = "*"}))
               (InfixExpression
                  (Label {labelUUID = UUID "7"})
                  (VariableExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "y"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "8"})
                      (Identifier {identifierString = "*"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "6"})
                     (Identifier {identifierString = "_"}))))))
  , Test
      "Infix completion (after nested function application of hole)"
      (typeChars "x*y +")
      (focus
         (UUID "6")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "7"})
               (InfixExpression
                  (Label {labelUUID = UUID "2"})
                  (VariableExpression
                     (Label {labelUUID = UUID "STARTER-EXPR"})
                     (Identifier {identifierString = "x"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "3"})
                      (Identifier {identifierString = "*"}))
                  (VariableExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "y"})))
               ( "+"
               , VariableExpression
                   (Label {labelUUID = UUID "8"})
                   (Identifier {identifierString = "+"}))
               (ConstantExpression
                  (Label {labelUUID = UUID "6"})
                  (Identifier {identifierString = "_"})))))
  , Test
      "Infix preserves precedence"
      (typeChars "*+/")
      (focus
         (UUID "7")
         (rhsSelectedState
            (InfixExpression
               (Label {labelUUID = UUID "5"})
               (InfixExpression
                  (Label {labelUUID = UUID "2"})
                  (ConstantExpression
                     (Label {labelUUID = UUID "STARTER-EXPR"})
                     (Identifier {identifierString = "_"}))
                  ( "*"
                  , VariableExpression
                      (Label {labelUUID = UUID "3"})
                      (Identifier {identifierString = "*"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "1"})
                     (Identifier {identifierString = "_"})))
               ( "+"
               , VariableExpression
                   (Label {labelUUID = UUID "6"})
                   (Identifier {identifierString = "+"}))
               (InfixExpression
                  (Label {labelUUID = UUID "8"})
                  (ConstantExpression
                     (Label {labelUUID = UUID "4"})
                     (Identifier {identifierString = "_"}))
                  ( "/"
                  , VariableExpression
                      (Label {labelUUID = UUID "9"})
                      (Identifier {identifierString = "/"}))
                  (ConstantExpression
                     (Label {labelUUID = UUID "7"})
                     (Identifier {identifierString = "_"}))))))
  ]

variableTests :: [Test]
variableTests =
  [ Test
      "Type variable name"
      (typeChars "foo")
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "foo"})))
  , Test
      "Ignore uppercase beginning"
      (typeChars "Foo")
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "oo"})))
  , Test
      "Type variable name, delete characters"
      (typeChars "foo" <> typeBackspace <> typeBackspace)
      (rhsSelectedState
         (VariableExpression
            (Label {labelUUID = starterExprUUID})
            (Identifier {identifierString = "f"})))
  , Test
      "Type variable name, empty back to constant"
      (typeChars "foo" <> typeBackspace <> typeBackspace <> typeBackspace)
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
