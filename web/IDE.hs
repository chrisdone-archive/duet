{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Control.Monad.State (execStateT, StateT, get, put, modify, runStateT)
import           Control.Monad.Trans
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
  | KeyDown !Keydown
  | KeyPress !Int
  | InsertChar !Char
  | PutExpression !(Expression Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Label = Label
  { labelUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Keydown
  = BackspaceKey
  | TabKey
  | DownKey
  | UpKey
  | LeftKey
  | RightKey
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  mstate <- Flux.Persist.getAppStateVal
  maybe (return ()) (Flux.alterStore store . ReplaceState) mstate
  Flux.Events.onBodyKeydown
    (\key ->
       case key of
         8 -> Flux.alterStore store (KeyDown BackspaceKey)
         9 -> Flux.alterStore store (KeyDown TabKey)
         37 -> Flux.alterStore store (KeyDown LeftKey)
         39 -> Flux.alterStore store (KeyDown RightKey)
         38 -> Flux.alterStore store (KeyDown UpKey)
         40 -> Flux.alterStore store (KeyDown DownKey)
         _ -> print key)
  Flux.Events.onBodyKeypress (Flux.alterStore store . KeyPress)
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
    KeyPress k -> interpretKeyPress k
    KeyDown k -> interpretKeyDown k
    ReplaceState s -> put s
    PutExpression e -> do
      s <- get
      case (stateAST s, stateCursor s) of
        (Just ast, Just cursor) -> do
          ast' <- transformNode (cursorNode cursor) (const (const (pure e))) ast
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
                   transformNode (cursorNode cursor) (const (pure . insertCharInto c)))
                  (stateAST s)
              put s {stateAST = ast}

interpretKeyDown :: Keydown -> StateT State IO ()
interpretKeyDown k = do
  s <- get
  case stateMode s of
    ExpressionMode ->
      case stateCursor s of
        Nothing -> pure ()
        Just cursor ->
          case k of
            BackspaceKey ->
              case stateAST s of
                Nothing -> pure ()
                Just ast -> interpretBackspace cursor ast
            _ -> pure ()

interpretBackspace :: Cursor -> Expression Ignore Identifier Label -> StateT State IO ()
interpretBackspace cursor ast = do
  (tweakedAST, parentOfDoomedChild) <-
    runStateT
      (transformNode
         (cursorNode cursor)
         (\mparent e -> do
            case e of
              VariableExpression l (Identifier string) -> do
                if length string > 1
                  then pure
                         (VariableExpression
                            l
                            (Identifier (take (length string - 1) string)))
                  else pure (ConstantExpression l (Identifier "_"))
              ConstantExpression l (Identifier "_") -> do
                put mparent
                pure e
              _ -> pure e)
         ast)
      Nothing
  astWithDeletion <-
    maybe
      (pure tweakedAST)
      (\uuid ->
         transformNode
           uuid
           (const
              (\case
                 ApplicationExpression l f x
                   | labelUUID (expressionLabel x) == cursorNode cursor -> do
                     focusNode (expressionLabel f)
                     pure f
                 CaseExpression _ e _
                   | labelUUID (expressionLabel e) == cursorNode cursor -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     pure w
                 e -> pure e))
           tweakedAST)
      parentOfDoomedChild
  modify (\s -> s {stateAST = Just astWithDeletion})

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
  (ast', parentEditAllowed) <-
    runStateT
      (transformNode
         (cursorNode cursor)
         (\_ f -> do
            case f of
              VariableExpression _ (Identifier "case") -> do
                c <- liftIO newCaseExpression
                case c of
                  CaseExpression _ e _ -> do
                    lift(focusNode (expressionLabel e))
                    put False
                  _ -> pure ()
                pure c
              _ -> do
                w <- liftIO newExpression
                lift (focusNode (expressionLabel w))
                liftIO (newApplicationExpression f w))
         ast)
      True
  ast'' <-
    if parentEditAllowed
      then do
        let mparent = findNodeParent (cursorNode cursor) ast
        case mparent of
          Just parent ->
            case parent of
              ApplicationExpression {} ->
                transformNode
                  (labelUUID (expressionLabel parent))
                  (\_ ap -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     liftIO (newApplicationExpression ap w))
                  ast
              _ -> pure ast'
          Nothing -> pure ast'
      else pure ast'
  modify (\s -> s {stateAST = Just ast''})

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

findNodeParent
  :: UUID
  -> Expression Ignore Identifier Label
  -> Maybe (Expression Ignore Identifier Label)
findNodeParent uuid = go Nothing
  where
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then mparent
        else case e of
               ApplicationExpression l e1 e2 ->
                 go (Just e) e1 <|> go (Just e) e2
               LambdaExpression l (Alternative al ps e') -> go (Just e) e'
               IfExpression l a b c ->
                 go (Just e) a <|> go (Just e) b <|> go (Just e) c
               CaseExpression l e' alts ->
                 go (Just e) e' <|> foldr (<|>) Nothing (map (\(x, k) -> go (Just e) k) alts)
               _ -> Nothing

transformNode
  :: Monad m
  => UUID
  -> (Maybe UUID -> Expression Ignore Identifier Label -> m (Expression Ignore Identifier Label))
  -> Expression Ignore Identifier Label
  -> m (Expression Ignore Identifier Label)
transformNode uuid f = go Nothing
  where
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then f (fmap labelUUID mparent) e
        else case e of
               ApplicationExpression l e1 e2 ->
                 ApplicationExpression l <$> go (Just l) e1 <*> go (Just l) e2
               LambdaExpression l (Alternative al ps e') ->
                 LambdaExpression l <$> (Alternative al ps <$> go (Just l) e')
               IfExpression l a b c ->
                 IfExpression l <$> go (Just l) a <*> go (Just l) b <*> go (Just l) c
               CaseExpression l e' alts ->
                 CaseExpression l <$> go (Just l) e' <*>
                 mapM (\(x, k) -> (x, ) <$> go (Just l) k) alts
               _ -> pure e

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
      renderExpr label "duet-variable" (Flux.elemText (T.pack ident))
    ApplicationExpression label f x ->
      renderExpr
        label
        "duet-application"
        (do case f of
              ApplicationExpression {} -> renderExpression mcursor f
              _ -> parens f (renderExpression mcursor f)
            Flux.elemText " "
            parens x (renderExpression mcursor x))
    ConstantExpression label (Identifier ident) ->
      renderExpr label "duet-constant" (Flux.elemText (T.pack ident))
    CaseExpression label e alts ->
      renderExpr
        label
        "duet-case"
        (do Flux.span_
              ["className" @= "duet-keyword", "key" @= "case"]
              (Flux.elemText "case")
            renderExpression mcursor e
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "of"]
              (Flux.elemText "of")
            mapM_
              (\(pat, expr) -> do
                 renderPattern mcursor pat
                 Flux.span_
                   ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
                   (Flux.elemText "→")
                 renderExpression mcursor expr)
              alts)
    _ -> pure ()
  where
    renderExpr label className' =
      renderNode mcursor label ("duet-expression " <> className')

parens
  :: Expression Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
parens e m =
  if atomic e
    then m
    else do
      Flux.span_
        ["className" @= "duet-parens", "key" @= "open-paren"]
        (Flux.elemText "(")
      m
      Flux.span_
        ["className" @= "duet-parens", "key" @= "close-paren"]
        (Flux.elemText ")")

atomic :: Expression t i l -> Bool
atomic e =
  case e of
    VariableExpression {} -> True
    ConstantExpression {} -> True
    CaseExpression {} -> False
    ApplicationExpression {} -> False
    IfExpression {} -> False
    LambdaExpression {} -> False
    LetExpression {} -> False
    InfixExpression {} -> False
    LiteralExpression {} -> True
    ConstructorExpression {} -> True

renderPattern
  :: Maybe Cursor
  -> Pattern Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
renderPattern mcursor =
  \case
    WildcardPattern label string ->
       renderNode mcursor label "duet-pattern duet-pattern-wildcard" (Flux.elemText (T.pack string))
    _ -> pure ()

renderNode
  :: Maybe Cursor
  -> Label
  -> Text
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
renderNode mcursor label className' =
  Flux.span_
    [ "key" @= labelUUID label
    , "className" @=
      (className' <> " " <>
       (if Just (labelUUID label) == fmap cursorNode mcursor
          then "duet-selected"
          else "duet-unselected"))
    ]
