{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Resolve type-class instances.

module Duet.Resolver where

import Control.Monad.Catch
import Control.Monad.Supply
import Data.Map.Strict (Map)
import Data.Maybe
import Duet.Infer
import Duet.Printer
import Duet.Renamer
import Duet.Types

resolveBindGroup
  :: (MonadSupply Int m, MonadThrow m ,Show l)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> BindGroup Type Name (TypeSignature Type Name l)
  -> m (BindGroup Type Name (TypeSignature Type Name l))
resolveBindGroup classes specialTypes (BindGroup explicit implicit) = do
  explicits <- mapM (resolveExplicit classes specialTypes) explicit
  implicits <- mapM (mapM (resolveImplicit classes specialTypes)) implicit
  pure (BindGroup explicits implicits)

resolveImplicit
  :: (MonadSupply Int m, MonadThrow m ,Show l)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> ImplicitlyTypedBinding Type Name (TypeSignature Type Name l)
  -> m (ImplicitlyTypedBinding Type Name (TypeSignature Type Name l))
resolveImplicit classes specialTypes (ImplicitlyTypedBinding l name alts) =
  ImplicitlyTypedBinding l name <$> mapM (resolveAlt classes specialTypes) alts

resolveExplicit
  :: (MonadSupply Int m, MonadThrow m ,Show l)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> ExplicitlyTypedBinding Type Name (TypeSignature Type Name l)
  -> m (ExplicitlyTypedBinding Type Name (TypeSignature Type Name l))
resolveExplicit classes specialTypes (ExplicitlyTypedBinding l name alts) =
  ExplicitlyTypedBinding l name <$> mapM (resolveAlt classes specialTypes) alts

resolveAlt
  :: (MonadSupply Int m, MonadThrow m, Show l)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> Alternative Type Name (TypeSignature Type Name l)
  -> m (Alternative Type Name (TypeSignature Type Name l))
resolveAlt classes specialTypes (Alternative l ps e) = do
  {-trace
    (unlines
       [ "Predicates: " ++ show predicates
       , "By instances: " ++ show (map (\p -> (byInst classes p)) predicates)
       , "Classes: " ++ show classes
       ])
    (return ())-}
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
  "?dict" ++ printIdentifier defaultPrint name

resolveExp
  :: (MonadThrow m, MonadSupply Int m, Show l)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> [(Predicate Type Name, Name)]
  -> Expression Type Name (TypeSignature Type Name l)
  -> m (Expression Type Name (TypeSignature Type Name l))

resolveExp classes specialTypes dicts = go
  where
    go =
      \case
        VariableExpression l i -> do
          dictArgs <- mapM (lookupDictionary l) predicates
          pure
            (foldl (ApplicationExpression l) (VariableExpression l i) dictArgs)
          where Forall _ (Qualified predicates _) = typeSignatureScheme l
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        LambdaExpression l0 (Alternative l vs b) ->
          LambdaExpression l0 <$> (Alternative l vs <$> go b)
        CaseExpression l e alts ->
          CaseExpression l <$> go e <*>
          mapM (\(p, e') -> fmap (p, ) (go e')) alts
        e@ConstructorExpression {} -> pure e
        e@ConstantExpression {} -> pure e
        IfExpression l a b c -> IfExpression l <$> go a <*> go b <*> go c
        e@LiteralExpression {} -> pure e
    lookupDictionary l p =
      (case byInst classes p of
         Just (_, dict) -> do
           pure (VariableExpression l (dictionaryName dict))
         Nothing ->
           case lookup p dicts of
             Nothing -> throwM (NoInstanceFor p)
             Just v -> pure (VariableExpression l v))
