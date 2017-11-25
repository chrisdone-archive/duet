{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State (execStateT, StateT, get, put, modify, runStateT)
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Char
import           Data.Data
import           Data.Generics (listify, everything, mkQ, extQ)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
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
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

data State = State
  { stateCursor :: !Cursor
  , stateAST :: !Node
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Node
  = ExpressionNode !(Expression Ignore Identifier Label)
  | DeclNode !(Decl Ignore Identifier Label)
  | BindingNode !(Identifier, Label)
  | PatternNode !(Pattern Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

nodeUUID :: Node -> UUID
nodeUUID = labelUUID . nodeLabel

nodeLabel :: Node -> Label
nodeLabel =
  \case
    ExpressionNode e -> expressionLabel e
    DeclNode d -> declLabel d
    BindingNode (_, d) -> d
    PatternNode p -> patternLabel p

data Cursor = Cursor
  { cursorUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Action
  = ReplaceState !State
  | KeyDown !Bool !Keydown
  | KeyPress !Int
  | InsertChar !Char
  | PutExpression !(Expression Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Label = Label
  { labelUUID :: UUID
  } deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

data Keydown
  = BackspaceKey
  | TabKey
  | DownKey
  | UpKey
  | LeftKey
  | RightKey
  | ReturnKey
  | OpenParens
  | CloseParens
  deriving (Generic, NFData, Show, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Main entry point

main :: IO ()
main = do
  mstate <- Flux.Persist.getAppStateVal
  maybe (return ()) (Flux.alterStore store . ReplaceState) mstate
  Flux.Events.onBodyKeydown
    (\shift key ->
       case key of
         8 -> Flux.alterStore store (KeyDown shift BackspaceKey)
         9 -> Flux.alterStore store (KeyDown shift TabKey)
         37 -> Flux.alterStore store (KeyDown shift LeftKey)
         39 -> Flux.alterStore store (KeyDown shift RightKey)
         38 -> Flux.alterStore store (KeyDown shift UpKey)
         40 -> Flux.alterStore store (KeyDown shift DownKey)
         57 -> Flux.alterStore store (KeyDown shift OpenParens)
         48 -> Flux.alterStore store (KeyDown shift CloseParens)
         13 -> Flux.alterStore store (KeyDown shift ReturnKey)
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
    { stateCursor = Cursor {cursorUUID = uuidI}
    , stateAST =
        DeclNode
          (BindGroupDecl
             (Label {labelUUID = uuidD})
             (BindGroup
              { bindGroupImplicitlyTypedBindings =
                  [ [ ImplicitlyTypedBinding
                      { implicitlyTypedBindingLabel =
                          Label (Flux.Persist.UUID "STARTER-BINDING")
                      , implicitlyTypedBindingId = (Identifier "_",Label uuidI)
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
    uuidI = Flux.Persist.UUID "STARTER-BINDING-ID"

--------------------------------------------------------------------------------
-- Model

instance Flux.StoreData State where
  type StoreAction State = Action
  transform action state = do
    putStrLn ("Action: " ++ show action)
    -- putStrLn ("State before: " ++ show state)
    state' <- execStateT (interpretAction action) state
    -- putStrLn ("State after: " ++ show state')
    _ <- forkIO (Flux.Persist.setAppStateVal state')
    pure state'

--------------------------------------------------------------------------------
-- Interpret actions

interpretAction :: Action -> StateT State IO ()
interpretAction =
  \case
    KeyPress k -> interpretKeyPress k
    KeyDown shift k -> interpretKeyDown shift k
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
            transformNode
              (cursorUUID cursor)
              (const (pure . insertCharInto c))
              (stateAST s)
          put s {stateAST = ast}

interpretKeyDown :: Bool -> Keydown -> StateT State IO ()
interpretKeyDown shift k = do
  s <- get
  case stateCursor s of
    cursor ->
      case k of
        BackspaceKey -> interpretBackspace cursor (stateAST s)
        TabKey -> do
          let getContinuing =
                if shift
                  then reverse . takeWhile ((/= me) . labelUUID)
                  else dropWhile ((== me) . labelUUID) .
                       dropWhile ((/= me) . labelUUID)
          maybe
            (return ())
            focusNode
            (listToMaybe (getContinuing (nodeHoles me (stateAST s))))
          where me = cursorUUID (stateCursor s)
        LeftKey ->
          navigate
            s
            (reverse . takeWhile ((/= cursorUUID (stateCursor s)) . labelUUID))
        RightKey ->
          navigate
            s
            (dropWhile ((== me) . labelUUID) . dropWhile ((/= me) . labelUUID))
          where me = cursorUUID (stateCursor s)
        ReturnKey -> interpretReturn (cursorUUID cursor) (stateAST s)
        OpenParens -> do
          ast <-
            transformExpression
              (cursorUUID cursor)
              (const
                 (\e ->
                    case e of
                      (ConstantExpression l (Identifier "_")) -> do
                        l' <- liftIO newParens
                        case l' of
                          ParensExpression l e' -> focusNode (expressionLabel e')
                          _ -> pure ()
                        pure l'
                      _ -> pure e))
              (stateAST s)
          modify (\s -> s {stateAST = ast})
        _ -> pure ()
  where
    navigate s skip =
      maybe
        (return ())
        focusNode
        (listToMaybe (skip (orderedNodes (stateAST s))))

interpretReturn :: UUID -> Node -> StateT State IO ()
interpretReturn uuid ast = do
  let me = findExpression uuid ast
  case me of
    Just e ->
      case e of
        CaseExpression l ce alts -> do
          ast' <-
            transformExpression
              uuid
              (\_ _ -> do
                 alt@(p,ex) <- liftIO newAlternative
                 focusNode (patternLabel p)
                 pure (CaseExpression l ce (alts ++ [alt])))
              ast
          modify (\s -> s {stateAST = ast'})
        _ -> goUp
    Nothing -> goUp
  where
    goUp = do
      let mparent = findNodeParent uuid ast
      maybe (pure ()) (flip interpretReturn ast . labelUUID . nodeLabel) mparent

findExpression :: UUID -> Node -> Maybe (Expression Ignore Identifier Label)
findExpression uuid =
  listToMaybe . listify ((== uuid) . labelUUID . expressionLabel)

interpretBackslash :: Cursor -> Node -> StateT State IO ()
interpretBackslash cursor ast = do
  ast' <-
    transformNode
      (cursorUUID cursor)
      (\mparent n ->
         case n of
           ExpressionNode (ConstantExpression l (Identifier "_")) -> do
             l <- liftIO newLambda
             case l of
               LambdaExpression _ (Alternative _ [p] _) ->
                 focusNode (patternLabel p)
               _ -> pure ()
             pure (ExpressionNode l)
           n -> pure n)
      ast
  modify (\s -> s {stateAST = ast'})

interpretBackspace :: Cursor -> Node -> StateT State IO ()
interpretBackspace cursor ast = do
  (tweakedAST, parentOfDoomedChild) <-
    runStateT
      (transformNode
         (cursorUUID cursor)
         (\mparent n -> do
            case n of
              ExpressionNode e ->
                fmap
                  ExpressionNode
                  (case e of
                     VariableExpression l (Identifier string) -> do
                       if length string > 1
                         then pure
                                (VariableExpression
                                   l
                                   (Identifier (take (length string - 1) string)))
                         else pure (ConstantExpression l (Identifier "_"))
                     LiteralExpression l lit ->
                       case lit of
                         IntegerLiteral i ->
                           let string = show i
                           in if length string > 1
                                then pure
                                       (LiteralExpression
                                          l
                                          (IntegerLiteral (read (init string))))
                                else pure
                                       (ConstantExpression l (Identifier "_"))
                         l -> pure e
                     ConstantExpression l (Identifier "_") -> do
                       put mparent
                       pure e
                     _ -> pure e)
              PatternNode e ->
                fmap
                  PatternNode
                  (case e of
                     VariablePattern l (Identifier string) -> do
                       if length string > 1
                         then pure
                                (VariablePattern
                                   l
                                   (Identifier (take (length string - 1) string)))
                         else pure (WildcardPattern l "_")
                     WildcardPattern l "_" -> do
                       put mparent
                       pure e
                     _ -> pure e)
              BindingNode (Identifier i, l) ->
                pure
                  (BindingNode
                     ( Identifier
                         (if length i > 1
                            then take (length i - 1) i
                            else "_")
                     , l))
              _ -> pure n)
         ast)
      Nothing
  astWithDeletion <-
    maybe
      (pure tweakedAST)
      (\uuid ->
         transformExpression
           uuid
           (const
              (\qq -> do
                 case qq of
                   ParensExpression _ x
                     | labelUUID (expressionLabel x) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   InfixExpression l x (_, op) y
                     | labelUUID (expressionLabel y) == cursorUUID cursor -> do
                       focusNode (expressionLabel x)
                       pure x
                     | labelUUID (expressionLabel x) == cursorUUID cursor -> do
                       focusNode (expressionLabel y)
                       pure y
                     | labelUUID (expressionLabel op) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
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
                   LambdaExpression _ (Alternative _ [p] _)
                     | labelUUID (patternLabel p) == cursorUUID cursor -> do
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
  case codeAsLetter k <|> codeAsDigit k of
    Nothing ->
      case k of
        92 -> interpretBackslash (stateCursor s) (stateAST s)
        42 -> interpretOperator '*' (stateCursor s) (stateAST s)
        43 -> interpretOperator '+' (stateCursor s) (stateAST s)
        45 -> interpretOperator '-' (stateCursor s) (stateAST s)
        47 -> interpretOperator '/' (stateCursor s) (stateAST s)
        _ ->
          case stateCursor s of
            cursor ->
              if isSpace k
                then interpretSpaceCompletion cursor (stateAST s)
                else pure ()
    Just c -> interpretAction (InsertChar c)
  where
    isSpace = (== 32)

interpretOperator :: Char -> Cursor -> Node -> StateT State IO ()
interpretOperator c cursor ast = do
  ast' <-
    transformExpression
      (cursorUUID cursor)
      (\mp e -> do
         w <- liftIO newExpression
         focusNode (expressionLabel w)
         liftIO (newInfixExpression c e w))
      ast
  modify (\s -> s {stateAST = ast'})

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

newInfixExpression
  :: Char
  -> Expression Ignore Identifier Label
  -> Expression Ignore Identifier Label
  -> IO (Expression Ignore Identifier Label)
newInfixExpression op x y = do
  uuid <- Flux.Persist.generateUUID
  uuid' <- Flux.Persist.generateUUID
  pure
    (InfixExpression
       (Label {labelUUID = uuid})
       x
       (pure op, VariableExpression (Label uuid') (Identifier (pure op)))
       y)

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
    fmap pure newAlternative

newLambda :: IO (Expression Ignore Identifier Label)
newLambda = do
  uuid <- Flux.Persist.generateUUID
  uuid2 <- Flux.Persist.generateUUID
  LambdaExpression (Label {labelUUID = uuid}) <$>
    (do (p, e) <- newAlternative
        pure (Alternative (Label {labelUUID = uuid2}) [p] e))

newAlternative :: IO (Pattern Ignore Identifier Label, Expression Ignore Identifier Label)
newAlternative = (,) <$> newPattern <*> newExpression

newParens :: IO (Expression Ignore Identifier Label)
newParens = do
  uuid <- Flux.Persist.generateUUID
  ParensExpression (Label {labelUUID = uuid}) <$> newExpression

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
    BindingNode d -> renderBinding cursor d
    PatternNode p -> renderPattern cursor p

renderDecl :: Cursor -> Decl Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderDecl cursor =
  \case
    BindGroupDecl label (BindGroup _ex implicit) ->
      renderWrap
        cursor
        label
        "diet-bind-group"
        (mapM_ (mapM_ (renderImplicitBinding cursor)) implicit)

renderImplicitBinding cursor (ImplicitlyTypedBinding label binding a) =
  renderWrap
    cursor
    label
    "duet-binding duet-implicit-binding"
    (mapM_ (renderAlternative cursor True (Just binding)) a)

renderBinding cursor (Identifier i, label') =
  renderWrap
    cursor
    label'
    ("duet-binding-name" <>
     if i == "_"
       then " duet-pattern-wildcard"
       else "")
    (Flux.elemText (T.pack i))

renderAlternative cursor equals mbinding (Alternative label pats e) =
  renderWrap
    cursor
    label
    "duet-alternative"
    (do maybe (return ()) (renderBinding cursor) mbinding
        mapM (renderPattern cursor) pats
        if not equals
          then Flux.span_
                 ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
                 (Flux.elemText "→")
          else Flux.span_
                 ["className" @= "duet-keyword", "key" @= "equals"]
                 (Flux.elemText "=")
        Flux.br_ ["key" @= "alt-break"]
        Flux.span_ ["className" @= "duet-rhs"] (renderExpression cursor e))

renderExpression
  :: Cursor
  -> Expression Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
renderExpression mcursor =
  \case
    VariableExpression label (Identifier ident) ->
      renderExpr label "duet-variable" (Flux.elemText (T.pack ident))
    LiteralExpression label lit -> renderLiteral mcursor label lit
    ParensExpression label e ->
      renderExpr
        label
        "duet-parens"
        (do Flux.span_
              [ "className" @= "duet-parens duet-open-parens"
              , "key" @= "open-paren"
              ]
              (Flux.elemText "(")
            renderExpression mcursor e
            Flux.span_
              ["className" @= "duet-parens", "key" @= "close-paren"]
              (Flux.elemText ")"))
    ApplicationExpression label f x ->
      renderExpr
        label
        "duet-application"
        (do case f of
              ApplicationExpression {} -> renderExpression mcursor f
              _ -> parens f (renderExpression mcursor f)
            parens x (renderExpression mcursor x))
    InfixExpression label f (_, op) x ->
      renderExpr
        label
        "duet-infix"
        (do renderExpression mcursor f
            Flux.span_
              ["className" @= "duet-op", "key" @= "op"]
              (renderExpression mcursor op)
            renderExpression mcursor x)
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
            Flux.br_ ["key" @= "of-rhs-break"]
            Flux.span_
              ["className" @= "duet-rhs"]
              (mapM_
                 (\(i, (pat, expr)) -> do
                    unless
                      (i == 1)
                      (Flux.br_ ["key" @= ("pat-break-" ++ show i)])
                    renderPattern mcursor pat
                    Flux.span_
                      [ "className" @= "duet-keyword duet-arrow"
                      , "key" @= ("arrow" ++ show i)
                      ]
                      (Flux.elemText "→")
                    Flux.br_ ["key" @= ("arrow-break-" ++ show i)]
                    Flux.span_
                      ["className" @= "duet-rhs", "key" @= ("rhs-" ++ show i)]
                      (renderExpression mcursor expr))
                 (zip [1 ..] alts)))
    LambdaExpression label (Alternative l ps e) ->
      renderExpr
        label
        "duet-lambda"
        (do Flux.span_
              ["className" @= "duet-lambda duet-keyword", "key" @= "backslash"]
              (Flux.elemText "\\")
            mapM_ (renderPattern mcursor) ps
            Flux.span_
              ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
              (Flux.elemText "→")
            Flux.br_ []
            Flux.span_ ["className" @= "duet-rhs"] (renderExpression mcursor e))
    _ -> pure ()
  where
    renderExpr label className' =
      renderWrap mcursor label ("duet-expression " <> className')

renderLiteral mcursor label lit =
  case lit of
    IntegerLiteral i ->
      renderExpr label "duet-integer" (Flux.elemText (T.pack (show i)))
    _ -> pure ()
  where renderExpr label className' =
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
    ParensExpression {} -> True

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
    VariablePattern label (Identifier string) ->
      renderWrap
        mcursor
        label
        "duet-pattern duet-pattern-variable"
        (Flux.elemText (T.pack string))
    LiteralPattern label lit -> renderLiteral mcursor  label lit
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
      ("duet-node " <> className' <> " " <>
       (if (labelUUID label) == cursorUUID mcursor
          then "duet-selected"
          else "duet-unselected"))
    ]

--------------------------------------------------------------------------------
-- Interpreter utilities

orderedNodes :: Node -> [Label]
orderedNodes =
  everything
    (++)
    (extQ
       (extQ
          (mkQ
             []
             (\(ImplicitlyTypedBinding l1 (_, l2) _ :: ImplicitlyTypedBinding Ignore Identifier Label) ->
                [l1, l2]))
          (\(e :: Expression Ignore Identifier Label) -> [expressionLabel e]))
       (\(p :: Pattern Ignore Identifier Label) -> [patternLabel p]))

nodeHoles :: UUID -> Node -> [Label]
nodeHoles base =
  everything
    (++)
    (extQ
       (extQ
          (mkQ
             []
             (\(i :: ImplicitlyTypedBinding Ignore Identifier Label) ->
                case i of
                  ImplicitlyTypedBinding _ (Identifier "_", label) _ -> [label]
                  ImplicitlyTypedBinding _ (_, l) _
                    | labelUUID l == base -> [l]
                  ImplicitlyTypedBinding l _ _
                    | labelUUID l == base -> [l]
                  _ -> []))
          (\(e :: Expression Ignore Identifier Label) ->
             case e of
               ConstantExpression {} -> [expressionLabel e]
               _
                 | labelUUID (expressionLabel e) == base -> [expressionLabel e]
                 | otherwise -> []))
       (\(p :: Pattern Ignore Identifier Label) ->
          case p of
            WildcardPattern l "_" -> [l]
            _
              | labelUUID (patternLabel p) == base -> [patternLabel p]
              | otherwise -> []))

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

insertCharInto :: Char -> Node -> Node
insertCharInto char =
  \case
    ExpressionNode n ->
      ExpressionNode
        (case n of
           VariableExpression l (Identifier s) ->
             VariableExpression l (Identifier (s ++ [char]))
           ConstantExpression l (Identifier "_")
             | letter -> VariableExpression l (Identifier [char])
             | digit -> LiteralExpression l (IntegerLiteral (read [char]))
           e -> e)
    BindingNode (Identifier "_", l)
      | letter -> BindingNode (Identifier [char], l)
    BindingNode (Identifier s, l) -> BindingNode (Identifier (s ++ [char]), l)
    PatternNode p ->
      PatternNode
        (case p of
           WildcardPattern l "_"
             | letter -> VariablePattern l (Identifier [char])
           WildcardPattern l "_"
             | digit -> LiteralPattern l (IntegerLiteral (read [char]))
           VariablePattern l (Identifier s) ->
             VariablePattern l (Identifier (s ++ [char]))
           p -> p)
    n -> n
  where
    letter = isLetter char
    digit = isDigit char

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
               BindingNode b -> goBinding mparent b
               PatternNode p -> goPat mparent p
               _ -> Nothing
    goPat mparent p =
      if labelUUID (patternLabel p) == uuid
        then mparent
        else Nothing
    goBinding mparent (_, l) =
      if labelUUID l == uuid
        then mparent
        else Nothing
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
               ParensExpression l e1 ->
                 go (Just (ExpressionNode e)) e1
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
               PatternNode d -> fmap PatternNode (goPat mparent d)
               BindingNode b -> fmap BindingNode (goBinding mparent b)
    goBinding mparent b@(i, l) =
      if labelUUID l == uuid
        then do
          n <- f (fmap labelUUID mparent) (BindingNode (i, l))
          case n of
            BindingNode b -> pure b
            _ -> pure b
        else pure b
    goPat mparent b =
      if labelUUID (patternLabel b) == uuid
        then do
          n <- f (fmap labelUUID mparent) (PatternNode b)
          case n of
            PatternNode b -> pure b
            _ -> pure b
        else pure b
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
                          ImplicitlyTypedBinding l <$> goBinding (Just l) i <*>
                          mapM (goAlt (Just l)) alts))
                    im)
               _ -> pure d
    goAlt mparent (Alternative l ps e) =
      Alternative l <$> mapM (goPat mparent) ps <*> go mparent e
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
               ParensExpression l e1 ->
                 ParensExpression l <$> go (Just l) e1
               InfixExpression l e1 (s, op) e2 ->
                 InfixExpression l <$> go (Just l) e1 <*>
                 ((s, ) <$> go (Just l) op) <*>
                 go (Just l) e2
               LambdaExpression l (Alternative al ps e') ->
                 LambdaExpression l <$>
                 (Alternative al <$> mapM (goPat (Just l)) ps <*> go (Just l) e')
               IfExpression l a b c ->
                 IfExpression l <$> go (Just l) a <*> go (Just l) b <*>
                 go (Just l) c
               CaseExpression l e' alts ->
                 CaseExpression l <$> go (Just l) e' <*>
                 mapM
                   (\(x, k) -> (,) <$> goPat (Just l) x <*> go (Just l) k)
                   alts
               _ -> pure e

codeAsLetter :: Int -> Maybe Char
codeAsLetter i =
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    then Just c
    else Nothing
  where
    c = toEnum i

codeAsDigit :: Int -> Maybe Char
codeAsDigit i =
  if (c >= '0' && c <= '9')
    then Just c
    else Nothing
  where
    c = toEnum i
