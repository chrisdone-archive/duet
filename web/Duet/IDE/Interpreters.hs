{-# LANGUAGE BangPatterns, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, LambdaCase, TupleSections, ExtendedDefaultRules, FlexibleContexts, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-type-defaults #-}

module Duet.IDE.Interpreters where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State (StateT, get, put, modify, runStateT, runState)
import           Control.Monad.Trans
import           Data.Char
import           Data.Generics (listify, everything, mkQ, extQ)
import           Data.Maybe
import           Duet.IDE.Constructors
import           Duet.IDE.Types
import           Duet.Types
import           React.Flux.Persist (UUID)

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
            (reverse .
             takeWhile ((/= cursorUUID (stateCursor s)) . labelUUID . nodeLabel))
        RightKey ->
          navigate
            s
            (dropWhile ((== me) . labelUUID . nodeLabel) .
             dropWhile ((/= me) . labelUUID . nodeLabel))
          where me = cursorUUID (stateCursor s)
        ReturnKey -> interpretReturn cursor (cursorUUID cursor) (stateAST s)
        _ -> pure ()
  where
    navigate s skip =
      maybe
        (return ())
        focusNode
        (listToMaybe
           (map
              nodeLabel
              (filter
                 isAtomicNode
                 (skip (orderedNodes (stateAST s))))))

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

interpretReturn :: Cursor -> UUID -> Node -> StateT State IO ()
interpretReturn cursor uuid ast = do
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
                 let (alts', inserted) =
                       runState
                         (fmap
                            concat
                            (mapM
                               (\a ->
                                  if isChildOf (cursorUUID cursor) a
                                    then do put True
                                            pure [a, alt]
                                    else pure [a])
                               alts))
                         False
                 pure
                   (CaseExpression
                      l
                      ce
                      (if inserted
                         then alts'
                         else alts' ++ [alt])))
              ast
          modify (\s -> s {stateAST = ast'})
        _ -> goUp
    Nothing -> goUp
  where
    isChildOf candidateUUID parent =
      not
        (null
           (listify
              ((== candidateUUID) . expressionUUID :: Expression Ignore Identifier Label -> Bool)
              parent))
    goUp = do
      let mparent = findNodeParent uuid ast
      maybe
        (pure ())
        (flip (interpretReturn cursor) ast . labelUUID . nodeLabel)
        mparent

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
              NameNode (Identifier "_", _) -> do
                put mparent
                pure n
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
                       focusNode (expressionLabel (rightMostExpression x))
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
                     | labelUUID (expressionLabel f) == cursorUUID cursor -> do
                       focusNode (expressionLabel x)
                       pure x
                   CaseExpression _ e _
                     | labelUUID (expressionLabel e) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   CaseExpression l e alts
                     | any predicate alts -> do
                       if length alts == 1
                         then do
                           w <- liftIO newExpression
                           focusNode (expressionLabel w)
                           pure w
                         else do
                           let focusCase = focusNode (expressionLabel e)
                           case alts of
                             [] -> focusCase
                             _ ->
                               let loop mlast [] =
                                     maybe focusCase focusNode mlast
                                   loop mlast (x:xs) =
                                     if predicate x
                                       then case mlast of
                                              Nothing ->
                                                maybe
                                                  focusCase
                                                  (focusNode .
                                                   expressionLabel .
                                                   caseAltExpression)
                                                  (listToMaybe xs)
                                              Just las -> focusNode (las)
                                       else loop
                                              (Just
                                                 (expressionLabel
                                                    (caseAltExpression x)))
                                              xs
                               in loop Nothing alts
                           pure (CaseExpression l e alts')
                     where alts' = filter (not . predicate) alts
                           predicate =
                             ((||) <$>
                              ((== cursorUUID cursor) .
                               (labelUUID . expressionLabel . caseAltExpression)) <*>
                              ((== cursorUUID cursor) .
                               (labelUUID . patternLabel . caseAltPattern)))
                   IfExpression _ e f g
                     | labelUUID (expressionLabel e) == cursorUUID cursor ||
                         labelUUID (expressionLabel f) == cursorUUID cursor ||
                         labelUUID (expressionLabel g) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   LambdaExpression _ (Alternative _ [p] _)
                     | labelUUID (patternLabel p) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   LambdaExpression _ (Alternative _ _ body)
                     | labelUUID (expressionLabel body) == cursorUUID cursor -> do
                       w <- liftIO newExpression
                       focusNode (expressionLabel w)
                       pure w
                   LambdaExpression l (Alternative l0 ps body)
                     | any predicate ps -> do
                       maybe
                         (return ())
                         (focusNode . patternLabel)
                         (listToMaybe
                            (reverse (takeWhile (not . predicate) ps) ++
                             drop 1 (dropWhile (not . predicate) ps)))
                       pure
                         (LambdaExpression
                            l
                            (Alternative l0 (filter (not . predicate) ps) body))
                     where predicate =
                             (== cursorUUID cursor) . labelUUID . patternLabel
                   e -> pure e))
           ast)
      parentOfDoomedChild
  modify (\s -> s {stateAST = astWithDeletion})

lastMaybe :: forall a. [a] -> Maybe a
lastMaybe xs =
  if null xs
    then Nothing
    else Just (last xs)

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
        40 -> interpretOpenParen s
        41 -> interpretCloseParen s
        _ ->
          case stateCursor s of
            cursor ->
              if isSpaceCode k
                then interpretSpaceCompletion cursor (stateAST s)
                else pure ()
    Just c -> interpretAction (InsertChar c)
  where
    isSpaceCode = (== 32)

interpretCloseParen :: Monad m => State -> StateT State m ()
interpretCloseParen s =
  case findNodeParent (cursorUUID (stateCursor s)) (stateAST s) of
    Nothing -> pure ()
    Just p ->
      case findNodeParent (nodeUUID p) (stateAST s) of
        Just p'@(ExpressionNode (ParensExpression{})) -> focusNode (nodeLabel p')
        _ -> focusNode (nodeLabel p)

interpretOpenParen :: MonadIO m => State -> StateT State m ()
interpretOpenParen s = do
  ast <-
    transformExpression
      (cursorUUID (stateCursor s))
      (const
         (\e ->
            case e of
              (ConstantExpression _ (Identifier "_")) -> do
                l' <- liftIO newParens
                case l' of
                  ParensExpression _ e' ->
                    focusNode (expressionLabel e')
                  _ -> pure ()
                pure l'
              LiteralExpression {} -> pure e
              _ -> do
                l' <- liftIO newParens
                case l' of
                  ParensExpression _ e' ->
                    focusNode (expressionLabel e')
                  _ -> pure ()
                liftIO (newApplicationExpression e l')))
      (stateAST s)
  modify (\s' -> s' {stateAST = ast})

interpretOperator :: Char -> Cursor -> Node -> StateT State IO ()
interpretOperator c cursor ast = do
  (ast', mparent) <-
    runStateT
      (transformNode
         (cursorUUID cursor)
         (\_ node ->
            case node of
              ExpressionNode e -> do
                if elem c ['+', '-']
                  then do
                    put (Just (widenExpressionInfixApps (dropWhileParens ast e) ast))
                    pure node
                  else do
                    put (Just (widenExpressionApps (dropWhileParens ast e) ast))
                    pure node
              n@(OperatorNode {}) -> pure (insertCharInto c n)
              n -> pure n)
         ast)
      Nothing
  case mparent of
    Nothing -> modify (\s -> s {stateAST = ast'})
    Just parent -> do
      ast'' <-
        transformNode
          (expressionUUID parent)
          (\_ _ -> do
             w <- liftIO newExpression
             focusNode (expressionLabel w)
             fmap ExpressionNode (liftIO (newInfixExpression c (peelLastHole parent) w)))
          ast
      modify (\s -> s {stateAST = ast''})

-- | Peel off layers of apps and infix to the right-hand-side hole.
peelLastHole :: Expression Ignore Identifier Label -> Expression Ignore Identifier Label
peelLastHole =
  \case
    ApplicationExpression _ f (ConstantExpression _ (Identifier "_")) ->
      peelLastHole f
    ApplicationExpression l f x -> ApplicationExpression l f (peelLastHole x)
    InfixExpression l x op y -> InfixExpression l x op (peelLastHole y)
    e -> e

-- | Climb the tree, dropping while we're looking at parens.
dropWhileParens ::
     Node
  -> Expression Ignore Identifier Label
  -> Expression Ignore Identifier Label
dropWhileParens ast =
  \case
    e@(ParensExpression {}) ->
      case findNodeParent (expressionUUID e) ast of
        Just (ExpressionNode parent) -> dropWhileParens ast parent
        _ -> e
    e -> e

-- | Widen an expression to the top-level function application.
--
-- a * f [x] becomes a * [f x]
-- f (k * [p]) remains f (k * [p])
widenExpressionApps
  :: Expression Ignore Identifier Label
  -> Node
  -> Expression Ignore Identifier Label
widenExpressionApps expression0 ast = go expression0
  where
    go expression =
      case expression of
        ApplicationExpression {} -> climb expression
        _
          | isAtomicExpression expression -> climb expression
          | otherwise -> expression
    climb expression =
      case findNodeParent (expressionUUID expression) ast of
        Just (ExpressionNode parent) ->
          case parent of
            ApplicationExpression {} -> go parent
            _ -> expression
        _ -> expression

-- | Widen an expression to the top-level infix application, but stop
-- at function application, or any syntax like if/case/etc.
--
-- a * f [x] becomes [a * f x]
-- f (k * [p]) becomes f ([k * p])
widenExpressionInfixApps
  :: Expression Ignore Identifier Label
  -> Node
  -> Expression Ignore Identifier Label
widenExpressionInfixApps expression0 ast = go True expression0
  where
    go ascendApplications expression =
      case expression of
        ApplicationExpression {}
          | ascendApplications -> climb expression ascendApplications
        InfixExpression {} -> climb expression False
        _
          | isAtomicExpression expression -> climb expression ascendApplications
          | otherwise -> expression
    climb expression ascendApplications =
      case findNodeParent (expressionUUID expression) ast of
        Just (ExpressionNode parent) ->
          case parent of
            ApplicationExpression {} -> go ascendApplications parent
            InfixExpression {} -> go ascendApplications parent
            _ -> expression
        _ -> expression

expressionUUID :: forall (t :: * -> *) i. Expression t i Label -> UUID
expressionUUID = labelUUID . expressionLabel

interpretSpaceCompletion :: Cursor -> Node -> StateT State IO ()
interpretSpaceCompletion cursor ast = do
  (ast', parentEditAllowed) <-
    runStateT
      (transformNode
         (cursorUUID cursor)
         (\_ n -> do
            case n of
              PatternNode {} -> do
                put True
                pure n
              ExpressionNode f ->
                fmap
                  ExpressionNode
                  (case f of
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
              _ -> pure n)
         ast)
      True
  ast'' <-
    if parentEditAllowed
      then do
        let mparent = findNodeParent (cursorUUID cursor) ast
        case mparent of
          Just parentNode ->
            case parentNode of
              ExpressionNode parent@(LambdaExpression l (Alternative l' ps b)) ->
                let (before, after) =
                      break
                        ((== cursorUUID cursor) . labelUUID . patternLabel)
                        ps
                in do new <- liftIO newPattern
                      focusNode (patternLabel new)
                      transformNode
                        (labelUUID (expressionLabel parent))
                        (\_ _ ->
                           pure
                             (ExpressionNode
                                (LambdaExpression
                                   l
                                   (Alternative
                                      l'
                                      (before ++
                                       take 1 after ++ [new] ++ drop 1 after)
                                      b))))
                        ast
              ExpressionNode parent@(ApplicationExpression _ f _)
                | cursorUUID cursor /= expressionUUID f ->
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
-- Interpreter utilities

orderedNodes :: Node -> [Node]
orderedNodes =
  everything
    (++)
    (extQ
       (extQ
          (mkQ
             []
             (\(ImplicitlyTypedBinding _ bind@(_, _) _ :: ImplicitlyTypedBinding Ignore Identifier Label) ->
                [NameNode bind]))
          (\(e :: Expression Ignore Identifier Label) -> [ExpressionNode e]))
       (\(p :: Pattern Ignore Identifier Label) -> [PatternNode p]))

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
    n@(NameNode (Identifier "_", l))
      | letter -> NameNode (Identifier [char], l)
      | otherwise -> n
    NameNode (Identifier s, l) -> NameNode (Identifier (s ++ [char]), l)
    OperatorNode l (Identifier "_")
     | operator -> OperatorNode l (Identifier [char])
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
    letter = isLetter char && isLower char
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
    goLamPat parent =
      \case
        VariablePattern l i -> goBinding parent (i, l)
        WildcardPattern l i -> goBinding parent (Identifier i, l)
        _ -> Nothing
    go mparent e =
      if labelUUID (expressionLabel e) == uuid
        then mparent
        else case e of
               ApplicationExpression _ e1 e2 ->
                 go (Just (ExpressionNode e)) e1 <|>
                 go (Just (ExpressionNode e)) e2
               InfixExpression _ e1 (_, o) e2 ->
                 go (Just (ExpressionNode e)) e1 <|>
                 go (Just (ExpressionNode e)) o <|>
                 go (Just (ExpressionNode e)) e2
               ParensExpression _ e1 -> go (Just (ExpressionNode e)) e1
               LambdaExpression _ (Alternative _ ps e') ->
                 foldr
                   (<|>)
                   Nothing
                   (map (goLamPat (Just (ExpressionNode e))) ps) <|>
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
            OperatorNode l' opident -> pure (opident, l')
            _ -> pure b
        else pure b
    goLamPat parent b =
      case b of
        VariablePattern l i -> do
          (Identifier i', l') <- goBinding parent (i, l)
          pure (if i' == "_"
                  then WildcardPattern l' i'
                  else VariablePattern l' (Identifier i'))
        WildcardPattern l i -> do
          (Identifier i', l') <- goBinding parent (Identifier i, l)
          pure
            (if i' == "_"
               then WildcardPattern l' i'
               else VariablePattern l' (Identifier i'))
        _ -> pure b
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
    go ::
         Maybe Label
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
                 (Alternative al <$> mapM (goLamPat (Just l)) ps <*>
                  go (Just l) e')
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

rightMostExpression :: Expression Ignore Identifier Label -> Expression Ignore Identifier Label
rightMostExpression x = fromMaybe x (lastMaybe (listify (const True) x))
