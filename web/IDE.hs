{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase #-}

module Main where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.State (execStateT, StateT, get, put, modify)
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
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
import qualified React.Flux.Persist as Flux.Persist

--------------------------------------------------------------------------------
-- Types

data State =
  State {stateMode :: Mode}
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Mode =
  ExpressionMode
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Action
  = ReplaceState !State
  | KeyDown !Int
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

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
store = Flux.mkStore State {stateMode = ExpressionMode}

--------------------------------------------------------------------------------
-- Model

instance Flux.StoreData State where
  type StoreAction State = Action
  transform action state = do
    putStrLn ("Action: " ++ show action)
    state' <- execStateT (interpretAction action) state
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    pure state'

--------------------------------------------------------------------------------
-- Interpret actions

interpretAction :: Action -> StateT State IO ()
interpretAction =
  \case
    KeyDown k -> interpretKeyPress k
    ReplaceState s -> put s

interpretKeyPress :: Int -> StateT State IO ()
interpretKeyPress k = do
  s <- get
  case stateMode s of
    ExpressionMode ->
      case codeAsLetter k of
        Nothing -> pure ()
        Just {} -> do uuid <- liftIO Flux.Persist.generateUUID
                      liftIO (print uuid)
                      pure ()

codeAsLetter :: Int -> Maybe Char
codeAsLetter i =
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then Just c
    else Nothing
  where
    c = toEnum i

--------------------------------------------------------------------------------
-- View

-- | The app's view.
appview :: State -> () -> ReactElementM ViewEventHandler ()
appview _ _ = pure ()
