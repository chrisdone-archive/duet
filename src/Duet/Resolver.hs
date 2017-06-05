{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Resolve type-class instances.

module Duet.Resolver where

import           Control.Monad.Catch
import           Control.Monad.Supply
import           Data.Map.Strict (Map)
import           Data.Maybe

import           Duet.Infer
import           Duet.Printer
import           Duet.Renamer
import           Duet.Types


resolveBindGroup
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Name l))
  -> SpecialTypes Name
  -> BindGroup Name (TypeSignature Name l)
  -> m (BindGroup Name (TypeSignature Name l))
resolveBindGroup classes specialTypes (BindGroup explicit implicit) = do
  explicits <- mapM (error "TODO: explicit bind groups not supported") explicit
  implicits <- mapM (mapM (resolveImplicit classes specialTypes)) implicit
  pure (BindGroup explicits implicits)

resolveImplicit
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Name l))
  -> SpecialTypes Name
  -> ImplicitlyTypedBinding Name (TypeSignature Name l)
  -> m (ImplicitlyTypedBinding Name (TypeSignature Name l))
resolveImplicit classes specialTypes (ImplicitlyTypedBinding l name alts) =
  ImplicitlyTypedBinding l name <$> mapM (resolveAlt classes specialTypes) alts

resolveAlt
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Name l))
  -> SpecialTypes Name
  -> Alternative Name (TypeSignature Name l)
  -> m (Alternative Name (TypeSignature Name l))
resolveAlt classes specialTypes (Alternative l ps e) = do
  dicts <-
    mapM
      (\pred' ->
         (pred', ) <$> supplyDictName (predicateToString specialTypes pred'))
      (filter (\p -> (not (isJust (byInst classes p)))) predicates)
  (Alternative l <$> pure ps <*>
   resolveExp
     classes
     specialTypes
     dicts
     (if null dicts
        then e
        else (LambdaExpression
                l
                (Alternative l [VariablePattern l d | (_, d) <- dicts] e))))
  where
    Forall _ (Qualified predicates _) = typeSignatureScheme l

predicateToString
  :: (Printable i, Show i)
  => SpecialTypes i -> Predicate Type i -> String
predicateToString specialTypes (IsIn name ts) =
  -- printIdentifier name ++ " " ++ unwords (map (printType specialTypes) ts)
  "$dict" ++ printIdentifier name

resolveExp
  :: (MonadThrow m, MonadSupply Int m)
  => Map Name (Class Type Name (TypeSignature Name l))
  -> SpecialTypes Name
  -> [(Predicate Type Name, Name)]
  -> Expression Name (TypeSignature Name l)
  -> m (Expression Name (TypeSignature Name l))
resolveExp classes specialTypes dicts = go
  where
    go =
      \case
        VariableExpression l i -> do
          dictArgs <- mapM (lookupDictionary l) predicates
          pure (foldl (ApplicationExpression l) (VariableExpression l i) dictArgs)
          where Forall _ (Qualified predicates _) = typeSignatureScheme l
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        LambdaExpression l0 (Alternative l vs b) ->
          LambdaExpression l0 <$> (Alternative l vs <$> go b)
        e -> pure e
    lookupDictionary l p =
      case byInst classes p of
        Just (_, dict) -> pure (VariableExpression l (dictionaryName dict))
        Nothing ->
          case lookup p dicts of
            Nothing -> throwM (NoInstanceFor p)
            Just v -> pure (VariableExpression l v)
