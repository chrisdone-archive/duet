{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables #-}
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
  { stateCursor :: !Cursor
  , stateAST :: !Node
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Node
  = ExpressionNode !(Expression Ignore Identifier Label)
  | DeclNode !(Decl Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

nodeUUID :: Node -> UUID
nodeUUID = labelUUID . nodeLabel

nodeLabel :: Node -> Label
nodeLabel =
  \case
    ExpressionNode e -> expressionLabel e
    DeclNode d -> declLabel d

data Cursor = Cursor
  { cursorUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

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
store = do
  Flux.mkStore
    State
    { stateCursor = Cursor {cursorUUID = uuidE}
    , stateAST =
        DeclNode
          (BindGroupDecl
             (Label {labelUUID = uuidD})
             (BindGroup
              { bindGroupImplicitlyTypedBindings =
                  [ [ ImplicitlyTypedBinding
                      { implicitlyTypedBindingLabel =
                          Label (Flux.Persist.UUID "STARTER-BINDING")
                      , implicitlyTypedBindingId = Identifier "_"
                      , implicitlyTypedBindingAlternatives =
                          [ Alternative
                            { alternativeLabel = Label (Flux.Persist.UUID "STARTER-ALT")
                            , alternativePatterns = []
                            , alternativeExpression =
                                ConstantExpression
                                  (Label {labelUUID = uuidE})
                                  (Identifier "_")
                            }
                          ]
                      }
                    ]
                  ]
              , bindGroupExplicitlyTypedBindings = []
              }))
    }
  where
    uuidE = Flux.Persist.UUID "STARTER-EXPR"
    uuidD = Flux.Persist.UUID "STARTER-DECL"

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
        (ast, cursor) -> do
          ast' <-
            transformExpression
              (cursorUUID cursor)
              (const (const (pure e)))
              ast
          modify (\s' -> s' {stateAST = ast'})
    InsertChar c -> do
      s <- get
      case stateCursor s of
        cursor -> do
          ast <-
            transformExpression
              (cursorUUID cursor)
              (const (pure . insertCharInto c))
              (stateAST s)
          put s {stateAST = ast}

interpretKeyDown :: Keydown -> StateT State IO ()
interpretKeyDown k = do
  s <- get
  case stateCursor s of
    cursor ->
      case k of
        BackspaceKey -> interpretBackspace cursor (stateAST s)
        TabKey -> do
          let mparent = findNodeParent (cursorUUID cursor) (stateAST s)
          case mparent of
            Nothing -> pure ()
            Just (ExpressionNode (CaseExpression l e ((alt, e'):alts))) ->
              focusNode (expressionLabel e')
        _ -> pure ()

interpretBackspace :: Cursor -> Node -> StateT State IO ()
interpretBackspace cursor ast = do
  (tweakedAST, parentOfDoomedChild) <-
    runStateT
      (transformExpression
         (cursorUUID cursor)
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
         transformExpression
           uuid
           (const
              (\case
                 ApplicationExpression l f x
                   | labelUUID (expressionLabel x) == cursorUUID cursor -> do
                     case f of
                       ApplicationExpression _ _ arg ->
                         focusNode (expressionLabel arg)
                       _ -> focusNode (expressionLabel f)
                     pure f
                 CaseExpression _ e _
                   | labelUUID (expressionLabel e) == cursorUUID cursor -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     pure w
                 IfExpression _ e _ _
                   | labelUUID (expressionLabel e) == cursorUUID cursor -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     pure w
                 e -> pure e))
           ast)
      parentOfDoomedChild
  modify (\s -> s {stateAST = astWithDeletion})

interpretKeyPress :: Int -> StateT State IO ()
interpretKeyPress k = do
  s <- get
  case codeAsLetter k of
    Nothing ->
      case stateCursor s of
        cursor ->
          if isSpace k
            then interpretSpaceCompletion cursor (stateAST s)
            else pure ()
    Just c -> interpretAction (InsertChar c)
  where
    isSpace = (== 32)

interpretSpaceCompletion :: Cursor -> Node -> StateT State IO ()
interpretSpaceCompletion cursor ast = do
  (ast', parentEditAllowed) <-
    runStateT
      (transformExpression
         (cursorUUID cursor)
         (\_ f -> do
            case f of
              VariableExpression _ (Identifier "if") -> do
                c <- liftIO newIfExpression
                case c of
                  IfExpression _ e _ _ -> do
                    lift (focusNode (expressionLabel e))
                    put False
                  _ -> pure ()
                pure c
              VariableExpression _ (Identifier "case") -> do
                c <- liftIO newCaseExpression
                case c of
                  CaseExpression _ e _ -> do
                    lift (focusNode (expressionLabel e))
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
        let mparent = findNodeParent (cursorUUID cursor) ast
        case mparent of
          Just parentNode ->
            case parentNode of
              ExpressionNode parent@ApplicationExpression {} ->
                transformExpression
                  (labelUUID (expressionLabel parent))
                  (\_ ap -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     liftIO (newApplicationExpression ap w))
                  ast
              _ -> pure ast'
          Nothing -> pure ast'
      else pure ast'
  modify (\s -> s {stateAST = ast''})

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
            (Cursor {cursorUUID = labelUUID l})
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
  -> Node
  -> Maybe Node
findNodeParent uuid = goNode Nothing
  where
    goNode mparent e =
      if nodeUUID e == uuid
        then mparent
        else case e of
               ExpressionNode e -> go mparent e
               DeclNode d -> goDecl mparent d
               _ -> Nothing
    goDecl mparent d =
      if labelUUID (declLabel d) == uuid
        then mparent
        else case d of
               BindGroupDecl _ (BindGroup ex im) ->
                 foldr
                   (<|>)
                   Nothing
                   (map (foldr (<|>) Nothing) (map (map (goIm mparent)) im))
    goIm mparent (ImplicitlyTypedBinding _ _ alts) =
      foldr (<|>) Nothing (map (goAlt mparent) alts)
    goAlt mparent (Alternative _ _ e) = go mparent e
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then mparent
        else case e of
               ApplicationExpression l e1 e2 ->
                 go (Just (ExpressionNode e)) e1 <|>
                 go (Just (ExpressionNode e)) e2
               LambdaExpression l (Alternative al ps e') ->
                 go (Just (ExpressionNode e)) e'
               IfExpression l a b c ->
                 go (Just (ExpressionNode e)) a <|>
                 go (Just (ExpressionNode e)) b <|>
                 go (Just (ExpressionNode e)) c
               CaseExpression l e' alts ->
                 go (Just (ExpressionNode e)) e' <|>
                 foldr
                   (<|>)
                   Nothing
                   (map (\(x, k) -> go (Just (ExpressionNode e)) k) alts)
               _ -> Nothing

transformExpression
  :: Monad m
  => UUID
  -> (Maybe UUID -> Expression Ignore Identifier Label -> m (Expression Ignore Identifier Label))
  -> Node
  -> m Node
transformExpression uuid f =
  transformNode
    uuid
    (\pid n ->
       case n of
         ExpressionNode e -> fmap ExpressionNode (f pid e)
         _ -> pure n)

transformNode
  :: forall m. Monad m
  => UUID
  -> (Maybe UUID -> Node -> m Node)
  -> Node
  -> m Node
transformNode uuid f = goNode Nothing
  where
    goNode :: Maybe Label -> Node -> m Node
    goNode mparent e =
      if nodeUUID e == uuid
        then f (fmap labelUUID mparent) e
        else case e of
               ExpressionNode e' -> fmap ExpressionNode (go mparent e')
               DeclNode d -> fmap DeclNode (goDecl mparent d)
    goDecl mparent d =
      if labelUUID (declLabel d) == uuid
        then do
          n <- f (fmap labelUUID mparent) (DeclNode d)
          case n of
            DeclNode d' -> pure d'
            _ -> pure d
        else case d of
               BindGroupDecl l (BindGroup ex im) ->
                 BindGroupDecl l <$>
                 (BindGroup ex <$>
                  mapM
                    (mapM
                       (\(ImplicitlyTypedBinding l i alts) ->
                          ImplicitlyTypedBinding l i <$>
                          mapM (goAlt (Just l)) alts))
                    im)
               _ -> pure d
    goAlt mparent (Alternative l ps e) =
      Alternative l <$> pure ps <*> go mparent e
    go
      :: Maybe Label
      -> Expression Ignore Identifier Label
      -> m (Expression Ignore Identifier Label)
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then do
          n <- f (fmap labelUUID mparent) (ExpressionNode e)
          case n of
            ExpressionNode e' -> pure e'
            _ -> pure e
        else case e of
               ApplicationExpression l e1 e2 ->
                 ApplicationExpression l <$> go (Just l) e1 <*> go (Just l) e2
               LambdaExpression l (Alternative al ps e') ->
                 LambdaExpression l <$> (Alternative al ps <$> go (Just l) e')
               IfExpression l a b c ->
                 IfExpression l <$> go (Just l) a <*> go (Just l) b <*>
                 go (Just l) c
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

newIfExpression :: IO (Expression Ignore Identifier Label)
newIfExpression = do
  uuid <- Flux.Persist.generateUUID
  IfExpression (Label {labelUUID = uuid}) <$> newExpression <*>
    newExpression <*> newExpression

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
  renderNode (stateCursor state) (stateAST state)

renderNode :: Cursor -> Node -> ReactElementM ViewEventHandler ()
renderNode cursor =
  \case
    ExpressionNode n -> renderExpression cursor n
    DeclNode d -> renderDecl cursor d

renderDecl :: Cursor -> Decl Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderDecl cursor =
  \case
    BindGroupDecl label (BindGroup _ex implicit) ->
      renderWrap
        cursor
        label
        "diet-bind-group"
        (mapM_ (mapM_ (renderImplicitBinding cursor)) implicit)

renderImplicitBinding cursor (ImplicitlyTypedBinding label (Identifier i) a) =
  renderWrap
    cursor
    label
    "duet-binding duet-implicit-binding"
    (do renderWrap
          cursor
          label
          ("duet-binding-name" <>
           if i == "_"
             then " duet-pattern-wildcard"
             else "")
          (Flux.elemText (T.pack i))
        mapM_ (renderAlternative cursor True) a)

renderAlternative cursor equals (Alternative label pats e) =
  renderWrap
    cursor
    label
    "duet-alternative"
    (do mapM (renderPattern cursor) pats
        if not equals
          then Flux.span_
                 ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
                 (Flux.elemText "→")
          else Flux.span_
                 ["className" @= "duet-keyword", "key" @= "equals"]
                 (Flux.elemText "=")
        renderExpression cursor e)

renderExpression
  :: Cursor
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
            parens x (renderExpression mcursor x))
    ConstantExpression label (Identifier ident) ->
      renderExpr label "duet-constant" (Flux.elemText (T.pack ident))
    IfExpression label e f g ->
      renderExpr
        label
        "duet-if"
        (do Flux.span_
              ["className" @= "duet-keyword", "key" @= "if"]
              (Flux.elemText "if")
            renderExpression mcursor e
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "then"]
              (Flux.elemText "then")
            renderExpression mcursor f
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "else"]
              (Flux.elemText "else")
            renderExpression mcursor g)
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
      renderWrap mcursor label ("duet-expression " <> className')

parens
  :: Expression Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
parens e m =
  if atomic e
    then m
    else do
      Flux.span_
        ["className" @= "duet-parens duet-open-parens", "key" @= "open-paren"]
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
  :: Cursor
  -> Pattern Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
renderPattern mcursor =
  \case
    WildcardPattern label string ->
      renderWrap
        mcursor
        label
        "duet-pattern duet-pattern-wildcard"
        (Flux.elemText (T.pack string))
    _ -> pure ()

renderWrap
  ::  Cursor
  -> Label
  -> Text
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
renderWrap mcursor label className' =
  Flux.span_
    [ "key" @= labelUUID label
    , "className" @=
      (className' <> " " <>
       (if (labelUUID label) == cursorUUID mcursor
          then "duet-selected"
          else "duet-unselected"))
    ]
