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
import           Duet.Printer (printImplicitlyTypedBinding, defaultPrint, PrintableType(..))
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

instance PrintableType Ignore where
  printType _ _ _ = ""

data State = State
  { stateCursor :: !Cursor
  , stateAST :: !Node
  } deriving (Generic, NFData, Show, FromJSON, ToJSON)

data Node
  = ExpressionNode !(Expression Ignore Identifier Label)
  | DeclNode !(Decl Ignore Identifier Label)
  | NameNode !(Identifier, Label)
  | OperatorNode !Label !Identifier
  | PatternNode !(Pattern Ignore Identifier Label)
  | AltNode !(CaseAlt Ignore Identifier Label)
  deriving (Generic, NFData, Show, FromJSON, ToJSON, Data, Typeable)

nodeUUID :: Node -> UUID
nodeUUID = labelUUID . nodeLabel

nodeLabel :: Node -> Label
nodeLabel =
  \case
    ExpressionNode e -> expressionLabel e
    DeclNode d -> declLabel d
    NameNode (_, d) -> d
    OperatorNode l _ -> l
    PatternNode p -> patternLabel p
    AltNode c -> caseAltLabel c

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
                      (ConstantExpression _ (Identifier "_")) -> do
                        l' <- liftIO newParens
                        case l' of
                          ParensExpression _ e' -> focusNode (expressionLabel e')
                          _ -> pure ()
                        pure l'
                      _ -> pure e))
              (stateAST s)
          modify (\s' -> s' {stateAST = ast})
        _ -> pure ()
  where
    navigate s skip =
      maybe
        (return ())
        focusNode
        (listToMaybe (skip (orderedNodes isAtomicNode (stateAST s))))

isAtomicNode :: Node -> Bool
isAtomicNode =
  \case
    ExpressionNode e -> isAtomicExpression e
    DeclNode {} -> False
    NameNode {} -> True
    OperatorNode {} -> True
    PatternNode p -> isAtomicPattern p
    AltNode {} -> False

isAtomicPattern :: Pattern t i l -> Bool
isAtomicPattern =
  \case
    VariablePattern {} -> True
    WildcardPattern {} -> True
    AsPattern {} -> True
    LiteralPattern {} -> True
    ConstructorPattern {} -> True

isAtomicExpression :: Expression t i l -> Bool
isAtomicExpression e =
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
    ParensExpression {} -> False

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
                 alt@(CaseAlt _ p _) <- liftIO newAlternative
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
      (\_ n ->
         case n of
           ExpressionNode (ConstantExpression _ (Identifier "_")) -> do
             l <- liftIO newLambda
             case l of
               LambdaExpression _ (Alternative _ [p] _) ->
                 focusNode (patternLabel p)
               _ -> pure ()
             pure (ExpressionNode l)
           _ -> pure n)
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
                         _ -> pure e
                     ConstantExpression _ (Identifier "_") -> do
                       let mp = findNodeParent (cursorUUID cursor) ast
                       case mp of
                         Just name@(AltNode {}) ->
                           maybe
                             (pure ())
                             (put . Just . nodeUUID)
                             (findNodeParent (nodeUUID name) ast)
                         _ -> put mparent
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
                     LiteralPattern l lit ->
                       case lit of
                         IntegerLiteral i ->
                           let string = show i
                           in if length string > 1
                                then pure
                                       (LiteralPattern
                                          l
                                          (IntegerLiteral (read (init string))))
                                else pure (WildcardPattern l "_")
                         _ -> pure e
                     WildcardPattern _ "_" -> do
                       let mp = findNodeParent (cursorUUID cursor) ast
                       case mp of
                         Just name@(AltNode {}) ->
                           maybe
                             (pure ())
                             (put . Just . nodeUUID)
                             (findNodeParent (nodeUUID name) ast)
                         _ -> put mparent
                       pure e
                     _ -> pure e)
              NameNode (Identifier i, l) ->
                pure
                  (NameNode
                     ( Identifier
                         (if length i > 1
                            then take (length i - 1) i
                            else "_")
                     , l))
              OperatorNode l (Identifier _) ->
                pure (OperatorNode l (Identifier "_"))
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
              (\en -> do
                 case en of
                   ParensExpression _ x
                     | labelUUID (expressionLabel x) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   InfixExpression _ x (_, op) y
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
                   ApplicationExpression _ f x
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
                   CaseExpression l e alts
                     | any predicate alts -> do
                       case alts of
                         [] -> focusNode (expressionLabel e)
                         (CaseAlt _ _ x:_) -> focusNode (expressionLabel x)
                       pure (CaseExpression l e alts')
                     where alts' = filter (not . predicate) alts
                           predicate =
                             ((||) <$>
                              ((== cursorUUID cursor) .
                               (labelUUID . expressionLabel . caseAltExpression)) <*>
                              ((== cursorUUID cursor) .
                               (labelUUID . patternLabel . caseAltPattern)))
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
              if isSpaceCode k
                then interpretSpaceCompletion cursor (stateAST s)
                else pure ()
    Just c -> interpretAction (InsertChar c)
  where
    isSpaceCode = (== 32)

interpretOperator :: Char -> Cursor -> Node -> StateT State IO ()
interpretOperator c cursor ast = do
  ast' <-
    transformNode
      (cursorUUID cursor)
      (\_ node ->
         case node of
           ExpressionNode e -> do
             w <- liftIO newExpression
             focusNode (expressionLabel w)
             fmap ExpressionNode (liftIO (newInfixExpression c e w))
           n@(OperatorNode {}) -> pure (insertCharInto c n)
           n -> pure n)
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
                  (\_ app -> do
                     w <- liftIO newExpression
                     focusNode (expressionLabel w)
                     liftIO (newApplicationExpression app w))
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
  LambdaExpression (Label {labelUUID = uuid}) <$>
    (do (CaseAlt l p e) <- newAlternative
        pure (Alternative l [p] e))

newAlternative :: IO (CaseAlt Ignore Identifier Label)
newAlternative = do
  uuid <- Flux.Persist.generateUUID
  CaseAlt (Label {labelUUID = uuid}) <$> newPattern <*> newExpression

newParens :: IO (Expression Ignore Identifier Label)
newParens = do
  uuid <- Flux.Persist.generateUUID
  ParensExpression (Label {labelUUID = uuid}) <$> newExpression

--------------------------------------------------------------------------------
-- View

-- | The app's view.
appview :: State -> () -> ReactElementM ViewEventHandler ()
appview state _ = do
  renderNode (stateCursor state) (stateAST state)
  Flux.p_
    ["key" @= "pretty-printed"]
    (Flux.text_
       (Flux.elemText
          (T.pack
             (case stateAST state of
                DeclNode (BindGroupDecl _ (BindGroup _ [[i]])) ->
                  printImplicitlyTypedBinding defaultPrint i
                _ -> "Nothing available to print."))))

renderNode :: Cursor -> Node -> ReactElementM ViewEventHandler ()
renderNode cursor =
  \case
    ExpressionNode n -> renderExpression cursor n
    DeclNode d -> renderDecl cursor d
    NameNode d -> renderBinding cursor d
    OperatorNode l d -> renderOperator cursor l d
    PatternNode p -> renderPattern cursor p
    AltNode _ -> pure ()

renderOperator :: forall eventHandler handler t. Flux.Term eventHandler [Flux.PropertyOrHandler handler] (ReactElementM ViewEventHandler () -> t) => Cursor -> Label -> Identifier -> t
renderOperator mcursor l op =
  Flux.span_
    ["className" @= "duet-op", "key" @= "op"]
    (renderExpression mcursor (VariableExpression l op))

renderDecl :: Cursor -> Decl Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderDecl cursor =
  \case
    BindGroupDecl label (BindGroup _ex implicit) ->
      renderWrap
        cursor
        label
        "diet-bind-group"
        (mapM_ (mapM_ (renderImplicitBinding cursor)) implicit)
    _ -> pure ()

renderImplicitBinding :: Cursor -> ImplicitlyTypedBinding Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderImplicitBinding cursor (ImplicitlyTypedBinding label binding a) =
  renderWrap
    cursor
    label
    "duet-binding duet-implicit-binding"
    (mapM_ (renderAlternative cursor True (Just binding)) a)

renderBinding :: Cursor -> (Identifier, Label) -> ReactElementM ViewEventHandler ()
renderBinding cursor (Identifier i, label') =
  renderWrap
    cursor
    label'
    ("duet-binding-name" <>
     if i == "_"
       then " duet-pattern-wildcard"
       else "")
    (Flux.elemText (T.pack i))

renderAlternative :: Cursor -> Bool -> Maybe (Identifier, Label) -> Duet.Types.Alternative Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderAlternative cursor equals mbinding (Alternative label pats e) =
  renderWrap
    cursor
    label
    "duet-alternative"
    (do maybe (return ()) (renderBinding cursor) mbinding
        mapM_ (renderPattern cursor) pats
        if not equals
          then Flux.span_
                 ["className" @= "duet-keyword duet-arrow", "key" @= "arrow"]
                 (Flux.elemText "→")
          else Flux.span_
                 ["className" @= "duet-keyword", "key" @= "equals"]
                 (Flux.elemText "=")
        Flux.br_ ["key" @= "alt-break"]
        Flux.span_
          ["className" @= "duet-rhs", "key" @= "alt-expr"]
          (renderExpression cursor e))

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
      renderExpr label "duet-parens" (renderExpression mcursor e)
    app@(ApplicationExpression label _ _) ->
      renderExpr
        label
        "duet-application"
        (do let (f, xs) = fargs app
            case f of
              ApplicationExpression {} -> renderExpression mcursor f
              _ -> parens "func" f (renderExpression mcursor f)
            if any lineBreaks xs
              then indented
                     "app"
                     (mapM_
                        (\(i, x) -> do
                           unless
                             (i == 1)
                             (Flux.br_ ["key" @= ("app-break-" ++ show i)])
                           parens
                             ("app-" ++ show i)
                             x
                             (renderExpression mcursor x))
                        (zip [1 ..] xs))
              else mapM_
                     (\(i, x) ->
                        parens ("app-" ++ show i) x (renderExpression mcursor x))
                     (zip [1 ..] xs))
    InfixExpression label f (_, VariableExpression l op) x ->
      renderExpr
        label
        "duet-infix"
        (do renderExpression mcursor f
            renderOperator mcursor l op
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
            renderExpressionIndented "if" mcursor e
            Flux.br_ ["key" @= "then-break"]
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "then"]
              (Flux.elemText "then")
            renderExpressionIndented "then" mcursor f
            Flux.br_ ["key" @= "else-break"]
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "else"]
              (Flux.elemText "else")
            renderExpressionIndented "else" mcursor g)
    CaseExpression label e alts ->
      renderExpr
        label
        "duet-case"
        (do Flux.span_
              ["className" @= "duet-keyword", "key" @= "case"]
              (Flux.elemText "case")
            if lineBreaks e
              then do
                renderExpressionIndented "case-expr" mcursor e
                Flux.br_ ["key" @= "else-break"]
              else renderExpression mcursor e
            Flux.span_
              ["className" @= "duet-keyword", "key" @= "of"]
              (Flux.elemText "of")
            Flux.br_ ["key" @= "of-rhs-break"]
            Flux.span_
              ["className" @= "duet-rhs", "key" @= "rhs"]
              (mapM_
                 (\(i, (CaseAlt l pat expr)) -> do
                    unless
                      (i == 1)
                      (Flux.br_ ["key" @= ("pat-break-" ++ show i)])
                    renderExpr
                      l
                      "duet-case-alt"
                      (do renderPattern mcursor pat
                          Flux.span_
                            [ "className" @= "duet-keyword duet-arrow"
                            , "key" @= ("arrow" ++ show i)
                            ]
                            (Flux.elemText "→")
                          Flux.br_ ["key" @= ("arrow-break-" ++ show i)]
                          Flux.span_
                            [ "className" @= "duet-rhs"
                            , "key" @= ("rhs-" ++ show i)
                            ]
                            (renderExpression mcursor expr)))
                 (zip [1 ..] alts)))
    LambdaExpression label (Alternative _ ps e) ->
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
            Flux.br_ ["key" @= "lambda-br"]
            Flux.span_
              ["className" @= "duet-rhs", "key" @= "lambda-rhs"]
              (renderExpression mcursor e))
    _ -> pure ()
  where
    renderExpr label className' =
      renderWrap mcursor label ("duet-expression " <> className')

-- | Flatten an application f x y into (f,[x,y]).
fargs :: Expression t i l -> (Expression t i l, [(Expression t i l)])
fargs e = go e []
  where
    go (ApplicationExpression _ f x) args = go f (x : args)
    go f args = (f, args)

renderExpressionIndented :: [Char] -> Cursor -> Expression Ignore Identifier Label -> ReactElementM ViewEventHandler ()
renderExpressionIndented prefix mcursor e =
  if lineBreaks e
    then indented prefix (renderExpression mcursor e)
    else renderExpression mcursor e

indented :: forall eventHandler handler t b handler1. Flux.Term eventHandler [Flux.PropertyOrHandler handler] (ReactElementM handler1 b -> t) => [Char] -> ReactElementM handler1 b -> t
indented prefix m = do
  Flux.span_
    ["key" @= (prefix ++ "-indented-wrap")]
    (do Flux.br_ ["key" @= (prefix ++ "-indented-break")]
        Flux.div_
          ["key" @= (prefix ++ "-indented-padding"), "className" @= "duet-indented"]
          m)

renderLiteral :: Cursor -> Label -> Literal -> ReactElementM ViewEventHandler ()
renderLiteral mcursor label lit =
  case lit of
    IntegerLiteral i ->
      renderExpr  "duet-integer" (Flux.elemText (T.pack (show i)))
    _ -> pure ()
  where renderExpr  className' =
              renderWrap mcursor label ("duet-expression " <> className')

lineBreaks :: Expression x y z -> Bool
lineBreaks =
  \case
    ApplicationExpression _ x y -> lineBreaks x || lineBreaks y
    InfixExpression _ x _ y -> lineBreaks x || lineBreaks y
    LambdaExpression {} -> True
    IfExpression {} -> True
    CaseExpression {} -> True
    ParensExpression _ e -> lineBreaks e
    _ -> False

parens
  :: String
  -> Expression Ignore Identifier Label
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
parens prefix e m =
  if atomic e
    then m
    else do
      Flux.span_
        [ "key" @= (prefix ++ "-parens")
        , "data-key" @= (prefix ++ "-parens")
        , "className" @= ("duet-node duet-implicit-parens")
        ]
        m

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
  :: Cursor
  -> Label
  -> Text
  -> ReactElementM ViewEventHandler ()
  -> ReactElementM ViewEventHandler ()
renderWrap mcursor label className' =
  Flux.span_
    [ "key" @= labelUUID label, "data-key" @= labelUUID label
    , "className" @=
      ("duet-node " <> className' <> " " <>
       (if (labelUUID label) == cursorUUID mcursor
          then "duet-selected"
          else "duet-unselected"))
    ]

--------------------------------------------------------------------------------
-- Interpreter utilities

orderedNodes :: (Node -> Bool) -> Node -> [Label]
orderedNodes ok =
  everything
    (++)
    (extQ
       (extQ
          (mkQ
             []
             (\(ImplicitlyTypedBinding _ bind@(_, l2) _ :: ImplicitlyTypedBinding Ignore Identifier Label) ->
                [l2 | ok (NameNode bind)]))
          (\(e :: Expression Ignore Identifier Label) ->
             [expressionLabel e | ok (ExpressionNode e)]))
       (\(p :: Pattern Ignore Identifier Label) ->
          [patternLabel p | ok (PatternNode p)]))

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
           LiteralExpression l (IntegerLiteral i)
             | digit -> LiteralExpression l (IntegerLiteral (read (show i ++ [char])))
           ConstantExpression l (Identifier "_")
             | letter -> VariableExpression l (Identifier [char])
             | digit -> LiteralExpression l (IntegerLiteral (read [char]))
           e -> e)
    NameNode (Identifier "_", l)
      | letter -> NameNode (Identifier [char], l)
    OperatorNode l (Identifier "_")
     | operator -> OperatorNode l (Identifier [char])
    NameNode (Identifier s, l) -> NameNode (Identifier (s ++ [char]), l)
    PatternNode p ->
      PatternNode
        (case p of
           WildcardPattern l "_"
             | letter -> VariablePattern l (Identifier [char])
           WildcardPattern l "_"
             | digit -> LiteralPattern l (IntegerLiteral (read [char]))
           LiteralPattern l (IntegerLiteral i)
             | digit -> LiteralPattern l (IntegerLiteral (read (show i ++ [char])))
           VariablePattern l (Identifier s) ->
             VariablePattern l (Identifier (s ++ [char]))
           _ -> p)
    n -> n
  where
    letter = isLetter char
    digit = isDigit char
    operator = elem char ("*/+-" :: [Char])

findNodeParent
  :: UUID
  -> Node
  -> Maybe Node
findNodeParent uuid = goNode Nothing
  where
    goNode mparent node =
      if nodeUUID node == uuid
        then mparent
        else case node of
               ExpressionNode e -> go mparent e
               DeclNode d -> goDecl mparent d
               NameNode b -> goBinding mparent b
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
               BindGroupDecl _ (BindGroup _ im) ->
                 foldr
                   (<|>)
                   Nothing
                   (map (foldr (<|>) Nothing) (map (map (goIm mparent)) im))
               _ -> Nothing
    goIm mparent (ImplicitlyTypedBinding _ _ alts) =
      foldr (<|>) Nothing (map (goAlt mparent) alts)
    goAlt mparent (Alternative _ _ e) = go mparent e
    goCaseAlt mparent ca@(CaseAlt l p e) =
      if labelUUID l == uuid
        then mparent
        else goPat (Just (AltNode ca)) p <|> go (Just (AltNode ca)) e
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then mparent
        else case e of
               ApplicationExpression _ e1 e2 ->
                 go (Just (ExpressionNode e)) e1 <|>
                 go (Just (ExpressionNode e)) e2
               ParensExpression _ e1 -> go (Just (ExpressionNode e)) e1
               LambdaExpression _ (Alternative _ _ e') ->
                 go (Just (ExpressionNode e)) e'
               IfExpression _ a b c ->
                 go (Just (ExpressionNode e)) a <|>
                 go (Just (ExpressionNode e)) b <|>
                 go (Just (ExpressionNode e)) c
               CaseExpression _ e' alts ->
                 go (Just (ExpressionNode e)) e' <|>
                 foldr
                   (<|>)
                   Nothing
                   (map
                      (\ca@(CaseAlt _ _ k) ->
                         goCaseAlt (Just (ExpressionNode e)) ca <|>
                         go (Just (AltNode ca)) k)
                      alts)
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
               NameNode b -> fmap NameNode (goBinding mparent b)
               AltNode {} -> pure e
               OperatorNode {} -> pure e
    goBinding mparent b@(i, l) =
      if labelUUID l == uuid
        then do
          n <- f (fmap labelUUID mparent) (NameNode (i, l))
          case n of
            NameNode name -> pure name
            _ -> pure b
        else pure b
    goOperator mparent b@(i, l) =
      if labelUUID l == uuid
        then do
          n <- f (fmap labelUUID mparent) (OperatorNode l i)
          case n of
            OperatorNode l' opident -> pure (opident,l')
            _ -> pure b
        else pure b
    goPat mparent b =
      if labelUUID (patternLabel b) == uuid
        then do
          n <- f (fmap labelUUID mparent) (PatternNode b)
          case n of
            PatternNode p -> pure p
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
                       (\(ImplicitlyTypedBinding l' i alts) ->
                          ImplicitlyTypedBinding l' <$> goBinding (Just l') i <*>
                          mapM (goAlt (Just l')) alts))
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
               ParensExpression l e1 -> ParensExpression l <$> go (Just l) e1
               InfixExpression l e1 (s, VariableExpression lop op) e2 ->
                 InfixExpression l <$> go (Just l) e1 <*>
                 ((s, ) <$>
                  fmap
                    (uncurry (flip VariableExpression))
                    (goOperator (Just l) (op, lop))) <*>
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
                   (\node@(CaseAlt l' x k) ->
                      if labelUUID l' == uuid
                        then do
                          n <- f (Just (labelUUID l)) (AltNode node)
                          case n of
                            AltNode an -> pure an
                            _ -> pure node
                        else CaseAlt l' <$> goPat (Just l') x <*> go (Just l') k)
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
