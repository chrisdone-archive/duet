{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

-- | The substitution stepper.

module Duet.Stepper where

import Control.Arrow
import Control.Monad.Catch
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Semigroup
import Duet.Types

--------------------------------------------------------------------------------
-- Expansion

expandSeq1
  :: MonadThrow m
  => SpecialSigs Name
  -> [TypeSignature Name Name]
  -> Expression Name (TypeSignature Name Location)
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
expandSeq1 specialSigs signatures e b = evalStateT (go e) False
  where
    go =
      \case
        e0
          | (ce@(ConstructorExpression l n), args) <- fargs e0 -> do
            args' <- mapM go args
            pure (foldl (ApplicationExpression l) ce args')
          | otherwise -> do
            alreadyExpanded <- get
            if alreadyExpanded
              then pure e0
              else do
                e <- lift (expandWhnf specialSigs signatures e0 b)
                put True
                pure e

expandWhnf
  :: MonadThrow m
  => SpecialSigs Name
  -> [TypeSignature Name Name]
  -> Expression Name (TypeSignature Name Location)
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
expandWhnf specialSigs signatures e b = go e
  where
    go x =
      case x of
        VariableExpression loc i -> do
          case find ((== i) . typeSignatureA) signatures of
            Nothing -> do
              e' <- lookupName i b
              pure e'
            Just {} -> pure x
        LiteralExpression {} -> return x
        ConstructorExpression {} -> return x
        ApplicationExpression l func arg ->
          case func of
            LambdaExpression l0 (Alternative l' params body) ->
              case params of
                (VariablePattern _ param:params') ->
                  let body' = substitute param arg body
                  in case params' of
                       [] -> pure body'
                       _ ->
                         pure
                           (LambdaExpression l0 (Alternative l' params' body'))
                [] -> error "Unsupported lambda."
            _ -> do
              func' <- go func
              pure (ApplicationExpression l func' arg)
        IfExpression l pr th el ->
          case pr of
            ConstructorExpression _ n
              | n == specialSigsTrue specialSigs -> pure th
              | n == specialSigsFalse specialSigs -> pure el
            _ -> IfExpression l <$> go pr <*> pure th <*> pure el
        InfixExpression {} -> return x
        LetExpression {} -> return x
        LambdaExpression {} -> return x
        CaseExpression l e0 alts ->
          let matches = map (first (match e0)) alts
          in case listToMaybe
                    (mapMaybe
                       (\(r, e) -> do
                          case r of
                            OK v -> pure (v, e)
                            Fail -> Nothing)
                       matches) of
               Just (Success subs, expr) ->
                 return
                   (foldr
                      (\(name, that) expr' -> substitute name that expr')
                      expr
                      subs)
               Just (NeedsMoreEval is, _) -> do
                 e' <- expandAt is specialSigs signatures e0 b
                 pure (CaseExpression l e' alts)
               Nothing -> error ("Incomplete pattern match... " ++ show matches)

expandAt
  :: MonadThrow m
  => [Int]
  -> SpecialSigs Name
  -> [TypeSignature Name Name]
  -> Expression Name (TypeSignature Name Location)
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
expandAt is specialSigs signatures e0 b = go [0] e0
  where
    go js e =
      if is == js
        then expandWhnf specialSigs signatures e b
        else case e of
               _
                 | (ce@(ConstructorExpression l _), args) <- fargs e -> do
                   args' <-
                     sequence
                       (zipWith (\i arg -> go (js ++ [i]) arg) [0 ..] args)
                   pure (foldl (ApplicationExpression l) ce args')
                 | otherwise -> pure e

--------------------------------------------------------------------------------
-- Pattern matching

match
  :: (Show l, Show i, Eq i)
  => Expression i l -> Pattern i l -> Result (Match i l)
match = go [0]
  where
    go is val pat =
      case pat of
        WildcardPattern _ -> OK (Success [])
        VariablePattern _ i -> OK (Success [(i, val)])
        ConstructorPattern _ i pats
          | (constructor@ConstructorExpression {}, args) <- fargs val ->
            if fmap (const ()) constructor == ConstructorExpression () i
              then if length args == length pats
                     then foldl
                            (<>)
                            (OK (Success []))
                            (zipWith
                               (\j (arg, p) -> go (is ++ [j]) arg p)
                               [0 ..]
                               (zip args pats))
                     else Fail
              else Fail
          | otherwise -> OK (NeedsMoreEval is)

--------------------------------------------------------------------------------
-- Expression manipulators

-- | Flatten an application f x y into (f,[x,y]).
fargs :: Expression i l -> (Expression i l, [(Expression i l)])
fargs e = go e []
  where
    go (ApplicationExpression _ f x) args = go f (x : args)
    go f args = (f, args)

--------------------------------------------------------------------------------
-- Substitutions

substitute :: Eq i => i -> Expression i l -> Expression i l -> Expression i l
substitute i arg =
  \case
    VariableExpression l i'
      | i == i' -> arg
      | otherwise -> VariableExpression l i'
    x@ConstructorExpression {} -> x
    ApplicationExpression l f x ->
      ApplicationExpression l (substitute i arg f) (substitute i arg x)
    InfixExpression l x f y ->
      InfixExpression l (substitute i arg x) f (substitute i arg y)
    LetExpression {} -> error "let expressions unsupported."
    CaseExpression l e cases ->
      CaseExpression
        l
        (substitute i arg e)
        (map (\(pat, e') -> (pat, substitute i arg e')) cases)
    IfExpression l a b c ->
      IfExpression
        l
        (substitute i arg a)
        (substitute i arg b)
        (substitute i arg c)
    x@LiteralExpression {} -> x
    LambdaExpression l (Alternative l' args body) ->
      LambdaExpression l (Alternative l' args (substitute i arg body))

--------------------------------------------------------------------------------
-- Lookups

lookupName
  :: (MonadThrow m)
  => Name
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
lookupName identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindName identifier)
    Just i -> pure i
  where
    findIdent (BindGroup _ is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ i [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is))

lookupNameByString
  :: (MonadThrow m)
  => String
  -> [BindGroup Name (TypeSignature Name Duet.Types.Location)]
  -> m (Expression Name (TypeSignature Name Location))
lookupNameByString identifier binds =
  case listToMaybe (mapMaybe findIdent binds) of
    Nothing -> throwM (CouldntFindNameByString identifier)
    Just i -> pure i
  where
    findIdent (BindGroup _ is) =
      listToMaybe
        (mapMaybe
           (\case
              ImplicitlyTypedBinding _ (ValueName _ i) [Alternative _ [] e]
                | i == identifier -> Just e
              _ -> Nothing)
           (concat is))
