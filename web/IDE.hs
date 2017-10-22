{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections #-}

module Main where

import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.State (execStateT, StateT, get, put, modify)
import           Data.Aeson
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Duet.Types
import           GHC.Generics
import           React.Flux ((@=))
import           React.Flux (ReactStore, ViewEventHandler, SomeStoreAction)
import qualified React.Flux as Flux
import qualified React.Flux.Events as Flux.Events
import           React.Flux.Internal (ReactElementM)
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
    PutExpression e -> do
      s <- get
      case (stateAST s, stateCursor s) of
        (Just ast, Just cursor) -> do
          ast' <- transformNode (cursorNode cursor) (const (pure e)) ast
          modify (\s' -> s' {stateAST = Just ast'})
        _ ->
          modify
            (\s' ->
               s'
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
            Just cursor -> do
              ast <-
                maybe
                  (pure Nothing)
                  (fmap Just .
                   transformNode (cursorNode cursor) (pure . insertCharInto c))
                  (stateAST s)
              put s {stateAST = ast}

interpretKeyPress :: Int -> StateT State IO ()
interpretKeyPress k = do
  s <- get
  case stateMode s of
    ExpressionMode ->
      case codeAsLetter k of
        Nothing ->
          case stateCursor s of
            Nothing -> pure ()
            Just cursor ->
              if isSpace k
                then case stateAST s of
                       Nothing -> pure ()
                       Just ast -> interpretSpaceCompletion cursor ast
                else pure ()
        Just c -> interpretAction (InsertChar c)
  where
    isSpace = (== 32)

interpretSpaceCompletion :: Cursor -> Expression Ignore Identifier Label -> StateT State IO ()
interpretSpaceCompletion cursor ast = do
  ast' <-
    transformNode
      (cursorNode cursor)
      (\f -> do
         case f of
           VariableExpression _ (Identifier "case") -> do
             c <- liftIO newCaseExpression
             case c of
               CaseExpression _ e _ -> focusNode (expressionLabel e)
               _ -> pure ()
             pure c
           _ -> do
             w <- liftIO newExpression
             focusNode (expressionLabel w)
             liftIO (newApplicationExpression f w))
      ast
  modify (\s -> s {stateAST = Just ast'})

--------------------------------------------------------------------------------
-- Interpreter utilities

focusNode
  :: Monad m
  => Label -> StateT State m ()
focusNode l =
  modify
    (\s ->
       s
       { stateCursor =
           Just (Cursor {cursorNode = labelUUID l})
       })

insertCharInto :: Char
               -> Expression Ignore Identifier Label
               -> Expression Ignore Identifier Label
insertCharInto char =
  \case
    VariableExpression l (Identifier s) ->
      VariableExpression l (Identifier (s ++ [char]))
    ConstantExpression l (Identifier "_") ->
      VariableExpression l (Identifier [char])
    ConstantExpression l (Identifier s) ->
      ConstantExpression l (Identifier (s ++ [char]))
    e -> e

transformNode
  :: Monad m
  => UUID
  -> (Expression Ignore Identifier Label -> m (Expression Ignore Identifier Label))
  -> Expression Ignore Identifier Label
  -> m (Expression Ignore Identifier Label)
transformNode uuid f e =
  if labelUUID (expressionLabel e) == uuid
    then f e
    else case e of
           ApplicationExpression l e1 e2 ->
             ApplicationExpression l <$> (go e1) <*> (go e2)
           LambdaExpression l (Alternative al ps e') ->
             LambdaExpression l <$> (Alternative al ps <$> go e')
           IfExpression l a b c ->
             IfExpression l <$> (go a) <*> (go b) <*> (go c)
           CaseExpression l e' alts ->
             CaseExpression l <$> (go e') <*>
             mapM (\(x, k) -> (x, ) <$> go k) alts
           _ -> pure e
  where
    go = transformNode uuid f

codeAsLetter :: Int -> Maybe Char
codeAsLetter i =
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then Just c
    else Nothing
  where
    c = toEnum i

--------------------------------------------------------------------------------
-- AST constructors

newExpression :: IO (Expression Ignore Identifier Label)
newExpression = do
  uuid <- Flux.Persist.generateUUID
  pure (ConstantExpression (Label {labelUUID = uuid}) (Identifier "_"))

newPattern :: IO (Pattern Ignore Identifier Label)
newPattern = do
  uuid <- Flux.Persist.generateUUID
  pure (WildcardPattern (Label {labelUUID = uuid}) "_")

newVariableExpression :: String -> IO (Expression Ignore Identifier Label)
newVariableExpression name = do
  uuid <- Flux.Persist.generateUUID
  pure (VariableExpression (Label {labelUUID = uuid}) (Identifier name))

newApplicationExpression
  :: Expression Ignore Identifier Label
  -> Expression Ignore Identifier Label
  -> IO (Expression Ignore Identifier Label)
newApplicationExpression x y = do
  uuid <- Flux.Persist.generateUUID
  pure (ApplicationExpression (Label {labelUUID = uuid}) x y)

newCaseExpression :: IO (Expression Ignore Identifier Label)
newCaseExpression = do
  uuid <- Flux.Persist.generateUUID
  CaseExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    fmap pure newAltlternative

newAltlternative :: IO (Pattern Ignore Identifier Label, Expression Ignore Identifier Label)
newAltlternative = (,) <$> newPattern <*> newExpression

--------------------------------------------------------------------------------
-- View

-- | The app's view.
appview :: State -> () -> ReactElementM ViewEventHandler ()
appview state _ =
  case stateAST state of
    Nothing -> pure ()
    Just ast -> renderExpression (stateCursor state) ast

renderExpression
  :: Maybe Cursor
  -> Expression Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
renderExpression mcursor =
  \case
    VariableExpression label (Identifier ident) ->
      Flux.span_
        [ "key" @= labelUUID label
        , "className" @=
          ("duet-variable " <>
           (if Just (labelUUID label) == fmap cursorNode mcursor
              then "duet-selected" :: Text
              else "duet-unselected"))
        ]
        (Flux.elemText (T.pack ident))
    ApplicationExpression label f x ->
      Flux.span_
        [ "key" @= labelUUID label
        , "className" @=
          ("duet-application " <>
           (if Just (labelUUID label) == fmap cursorNode mcursor
              then "duet-selected" :: Text
              else "duet-unselected"))
        ]
        (do renderExpression mcursor f
            Flux.elemText " "
            renderExpression mcursor x)
    ConstantExpression label (Identifier ident) ->
      Flux.span_
        [ "key" @= labelUUID label
        , "className" @=
          ("duet-constant " <>
           (if Just (labelUUID label) == fmap cursorNode mcursor
              then "duet-selected" :: Text
              else "duet-unselected"))
        ]
        (Flux.elemText (T.pack ident))
    _ -> pure ()
