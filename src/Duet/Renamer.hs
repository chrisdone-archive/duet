{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

-- At each binding point (lambdas), we need to supply a new unique
-- name, and then rename everything inside the expression.
--
-- For each BindGroup, we should generate the list of unique names
-- first for each top-level thing (which might be mutually
-- independent), and then run the sub-renaming processes, with the new
-- substitutions in scope.
--
-- It's as simple as that.

module Duet.Renamer where

import           Control.Arrow
import           Control.Monad.Catch
import           Control.Monad.Supply
import           Control.Monad.Trans
import           Control.Monad.Writer
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Duet.Types

--------------------------------------------------------------------------------
-- Perform renaming

instance MonadThrow m => MonadThrow (SupplyT Int m) where
  throwM = lift . throwM

renameBindGroups
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> [BindGroup Identifier l]
  -> m ([BindGroup Name l], Map Identifier Name)
renameBindGroups subs groups = do
  subs' <-
    fmap
      mconcat
      (mapM (\(BindGroup ex implicit) -> getImplicitSubs subs implicit) groups) -- TODO: explicit
  fmap (second mconcat . unzip) (mapM (renameBindGroup subs') groups)

renameBindGroup
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name
  -> BindGroup Identifier l
  -> m (BindGroup Name l, Map Identifier Name)
renameBindGroup subs (BindGroup explicit implicit) = do
  bindGroup' <-
    BindGroup <$> renameExplicit subs explicit <*>
    mapM (mapM (renameImplicit subs)) implicit
  pure (bindGroup', subs)

getImplicitSubs
  :: (MonadSupply Int m)
  => Map Identifier Name
  -> [[ImplicitlyTypedBinding Identifier l]]
  -> m (Map Identifier Name)
getImplicitSubs subs implicit =
  fmap
    ((<> subs) . M.fromList)
    (mapM
       (\(ImplicitlyTypedBinding _ i _) -> fmap (i, ) (supplyName i))
       (concat implicit))

renameExplicit :: Applicative f => t2 -> t1 -> f [t]
renameExplicit _ _ = pure [] -- TODO:

renameImplicit
  :: (MonadThrow m,MonadSupply Int m)
  => Map Identifier Name
  -> ImplicitlyTypedBinding Identifier t
  -> m (ImplicitlyTypedBinding Name t)
renameImplicit subs (ImplicitlyTypedBinding l id' alts) =
  do name <- substitute subs id'
     ImplicitlyTypedBinding l name <$> mapM (renameAlt subs) alts

renameAlt
  :: (MonadSupply Int m, MonadThrow m)
  => Map Identifier Name -> Alternative Identifier l -> m (Alternative Name l)
renameAlt subs (Alternative l ps e) =
  do (ps', subs') <- runWriterT (mapM renamePattern ps)
     let subs'' = M.fromList subs' <> subs
     Alternative l <$> pure ps' <*> renameExpression subs'' e

renamePattern
  :: MonadSupply Int m
  => Pattern Identifier
  -> WriterT [(Identifier, Name)] m (Pattern Name)
renamePattern =
  \case
    VariablePattern i -> do
      name <- lift (supplyName i)
      tell [(i, name)]
      pure (VariablePattern name)
    WildcardPattern -> pure WildcardPattern
    AsPattern i p -> do
      name <- supplyName i
      tell [(i, name)]
      AsPattern name <$> renamePattern p
    LiteralPattern l -> pure (LiteralPattern l)
    ConstructorPattern {} -> error "TODO: ConstructorPattern"

renameExpression
  :: (MonadThrow m, MonadSupply Int m)
  => Map Identifier Name -> Expression Identifier l -> m (Expression Name l)
renameExpression subs = go
  where
    go =
      \case
        VariableExpression l i -> VariableExpression l <$> substitute subs i
        LiteralExpression l i -> pure (LiteralExpression l i)
        ConstantExpression {} -> error "TODO: ConstantExpression"
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        InfixExpression l x i y ->
          InfixExpression l <$> go x <*> substitute subs i <*> go y
        LetExpression l bindGroup@(BindGroup ex implicit) e -> do
          subs' <- getImplicitSubs subs implicit
          (bindGroup', subs'') <- renameBindGroup subs' bindGroup
          LetExpression l <$> pure bindGroup' <*> renameExpression subs'' e
        LambdaExpression l alt -> LambdaExpression l <$> renameAlt subs alt
        IfExpression l x y z -> IfExpression l <$> go x <*> go y <*> go z
        CaseExpression l e pat_exps ->
          CaseExpression l <$> go e <*>
          mapM
            (\(pat, ex) -> do
               (pat', subs') <- runWriterT (renamePattern pat)
               e' <- renameExpression (M.fromList subs' <> subs) ex
               pure (pat', e'))
            pat_exps

--------------------------------------------------------------------------------
-- Provide a substitution

substitute :: MonadThrow m => Map Identifier Name -> Identifier -> m Name
substitute subs i =
  case M.lookup i subs of
    Nothing -> throwM (IdentifierNotInScope subs i)
    Just name -> pure name

--------------------------------------------------------------------------------
-- Provide a new name

supplyName :: (MonadSupply Int m) => Identifier -> m Name
supplyName (Identifier s) = do
  i <- supply
  return (NameFromSource i s)
