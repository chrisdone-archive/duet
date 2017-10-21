{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}

module Main where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.State (execStateT, StateT, get, put, modify)
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Types
import           GHC.Generics
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal (FromJSVal(..), ToJSVal(..), toJSVal_aeson)
import           GHCJS.Types (JSVal, JSString)
import           React.Flux ((@=))
import           React.Flux (ReactStore, ViewEventHandler, SomeStoreAction, ReactView)
import qualified React.Flux as Flux
import qualified React.Flux.Events as Flux.Events
import           React.Flux.Internal (ReactElementM)
import qualified React.Flux.Lifecycle as Flux.Lifecycle
import           React.Flux.Persist (UUID)
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- Types

data Ignore a = Ignore
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data State = State
  { stateMode :: Mode
  , stateCursor :: Maybe Cursor
  , stateAST :: Maybe (Expression Ignore Identifier Label)
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Cursor = Cursor
  { cursorNode :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Mode =
  ExpressionMode
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Action
  = ReplaceState !State
  | KeyDown !Int
  | InsertChar !Char
  | PutExpression !(Expression Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Label = Label
  { labelUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  mstate <- Flux.Persist.getAppStateVal
  maybe (return ()) (Flux.alterStore store . ReplaceState) mstate
  Flux.Events.onBodyKeydown (Flux.alterStore store . KeyDown)
  Flux.reactRender "app" (Flux.defineControllerView "State" store appview) ()

--------------------------------------------------------------------------------
-- Store setup

-- | Dispatch an action on the store.
dispatch :: Action -> SomeStoreAction
dispatch a = Flux.SomeStoreAction store a

-- | The app's model.
store :: ReactStore State
store =
  Flux.mkStore
    State
    {stateMode = ExpressionMode, stateCursor = Nothing, stateAST = Nothing}

--------------------------------------------------------------------------------
-- Model

instance Flux.StoreData State where
  type StoreAction State = Action
  transform action state = do
    putStrLn ("Action: " ++ show action)
    putStrLn ("State before: " ++ show state)
    state' <- execStateT (interpretAction action) state
    putStrLn ("State after: " ++ show state')
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    pure state'

--------------------------------------------------------------------------------
-- Interpret actions

interpretAction :: Action -> StateT State IO ()
interpretAction =
  \case
    KeyDown k -> interpretKeyPress k
    ReplaceState s -> put s
    PutExpression e ->
      modify
        (\s ->
           s
           { stateCursor =
               Just (Cursor {cursorNode = labelUUID (expressionLabel e)})
           , stateAST = Just e
           })
    InsertChar c -> do
      s <- get
      case stateMode s of
        ExpressionMode ->
          case stateCursor s of
            Nothing -> do
              e <- liftIO (newVariableExpression [c])
              interpretAction (PutExpression e)

interpretKeyPress :: Int -> StateT State IO ()
interpretKeyPress k = do
  s <- get
  case stateMode s of
    ExpressionMode ->
      case codeAsLetter k of
        Nothing -> pure ()
        Just c -> interpretAction (InsertChar c)

codeAsLetter :: Int -> Maybe Char
codeAsLetter i =
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then Just c
    else Nothing
  where
    c = toEnum i

--------------------------------------------------------------------------------
-- AST constructors

newVariableExpression :: String -> IO (Expression Ignore Identifier Label)
newVariableExpression name = do
  uuid <- Flux.Persist.generateUUID
  pure (VariableExpression (Label {labelUUID = uuid}) (Identifier name))

--------------------------------------------------------------------------------
-- View

-- | The app's view.
appview :: State -> () -> ReactElementM ViewEventHandler ()
appview _ _ = pure ()
