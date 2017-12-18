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
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Duet.Infer
import           Duet.Printer
import           Duet.Supply
import           Duet.Types

resolveTypeClasses
  :: (MonadSupply Int f, MonadThrow f)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> f (Map Name (Class Type Name (TypeSignature Type Name l)))
resolveTypeClasses typeClasses specialTypes = go typeClasses
  where
    go =
      fmap M.fromList .
      mapM
        (\(name, cls) -> do
           is <-
             mapM
               (\inst -> do
                  ms <-
                    mapM
                      (\(nam, (l, alt)) ->
                         fmap ((nam, ) . (l, )) (resolveAlt typeClasses specialTypes alt))
                      (M.toList (dictionaryMethods (instanceDictionary inst)))
                  pure
                    inst
                    { instanceDictionary =
                        (instanceDictionary inst)
                        {dictionaryMethods = M.fromList ms}
                    })
               (classInstances cls)
           pure (name, cls {classInstances = is})) .
      M.toList

resolveBindGroup
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> BindGroup Type Name (TypeSignature Type Name l)
  -> m (BindGroup Type Name (TypeSignature Type Name l))
resolveBindGroup classes specialTypes (BindGroup explicit implicit) = do
  explicits <- mapM (resolveExplicit classes specialTypes) explicit
  implicits <- mapM (mapM (resolveImplicit classes specialTypes)) implicit
  pure (BindGroup explicits implicits)

resolveImplicit
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> ImplicitlyTypedBinding Type Name (TypeSignature Type Name l)
  -> m (ImplicitlyTypedBinding Type Name (TypeSignature Type Name l))
resolveImplicit classes specialTypes (ImplicitlyTypedBinding l name alts) =
  ImplicitlyTypedBinding l name <$> mapM (resolveAlt classes specialTypes) alts

resolveExplicit
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> ExplicitlyTypedBinding Type Name (TypeSignature Type Name l)
  -> m (ExplicitlyTypedBinding Type Name (TypeSignature Type Name l))
resolveExplicit classes specialTypes (ExplicitlyTypedBinding l scheme name alts) =
  ExplicitlyTypedBinding l scheme name <$> mapM (resolveAlt classes specialTypes) alts

resolveAlt
  :: (MonadSupply Int m, MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> Alternative Type Name (TypeSignature Type Name l)
  -> m (Alternative Type Name (TypeSignature Type Name l))
resolveAlt classes specialTypes (Alternative l ps e) = do
  dicts <-
    mapM
      (\pred' ->
         (pred', ) <$> supplyDictName (predicateToString specialTypes pred'))
      (filter (\p -> (not (isJust (byInst classes p)))) (nub predicates))
  (Alternative l <$> pure ps <*>
   resolveExp
     classes
     specialTypes
     dicts
     (if null dicts
        then e
        else let dictArgs = [VariablePattern l d | (_, d) <- dicts]
             in case e of
                  LambdaExpression _ (Alternative l0 args e0) ->
                    LambdaExpression l (Alternative l0 (dictArgs ++ args) e0)
                  _ -> LambdaExpression l (Alternative l dictArgs e)))
  where
    Forall _ (Qualified predicates _) = typeSignatureScheme l

predicateToString
  :: (Printable i)
  => SpecialTypes i -> Predicate Type i -> String
predicateToString _specialTypes (IsIn name _ts) =
  -- printIdentifier name ++ " " ++ unwords (map (printType specialTypes) ts)
  "?dict" ++ printIdentifier defaultPrint name

resolveExp
  :: (MonadThrow m)
  => Map Name (Class Type Name (TypeSignature Type Name l))
  -> SpecialTypes Name
  -> [(Predicate Type Name, Name)]
  -> Expression Type Name (TypeSignature Type Name l)
  -> m (Expression Type Name (TypeSignature Type Name l))
resolveExp classes _ dicts = go
  where
    go =
      \case
        ParensExpression l e -> ParensExpression l <$> go e
        VariableExpression l i -> do
          dictArgs <- fmap concat (mapM (lookupDictionary l) predicates)
          pure
            (foldl (ApplicationExpression l) (VariableExpression l i) dictArgs)
          where Forall _ (Qualified predicates _) = typeSignatureScheme l
        ApplicationExpression l f x -> ApplicationExpression l <$> go f <*> go x
        InfixExpression l x (i, op) y ->
          InfixExpression l <$> go x <*> fmap (i, ) (go op) <*> go y
        LambdaExpression l0 (Alternative l vs b) ->
          LambdaExpression l0 <$> (Alternative l vs <$> go b)
        CaseExpression l e alts ->
          CaseExpression l <$> go e <*>
          mapM (\(CaseAlt l' p e') -> fmap (CaseAlt l' p) (go e')) alts
        e@ConstructorExpression {} -> pure e
        e@ConstantExpression {} -> pure e
        IfExpression l a b c -> IfExpression l <$> go a <*> go b <*> go c
        e@LiteralExpression {} -> pure e
    lookupDictionary l p =
      (case byInst classes p of
         Just (preds, dict) -> do
           do parents <- fmap concat (mapM (lookupDictionary l) preds)
              pure (VariableExpression l (dictionaryName dict) : parents)
         Nothing ->
           case lookup p dicts of
             Nothing -> throwM (NoInstanceFor p)
             Just v -> pure [VariableExpression l v])
