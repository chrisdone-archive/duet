{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Duet.Supply where

import Control.Monad.Catch
import Control.Monad.Supply
import Duet.Types

supplyValueName :: (MonadSupply Int m, Identifiable i, MonadThrow m) => i -> m Name
supplyValueName s = do
  i <- supply
  Identifier s' <- identifyValue s
  return (ValueName i s')

supplyConstructorName :: (MonadSupply Int m) => Identifier -> m Name
supplyConstructorName (Identifier s) = do
  i <- supply
  return (ConstructorName i s)

supplyDictName :: (MonadSupply Int m) => String -> m Name
supplyDictName s = do
  i <- supply
  return (DictName i s)

supplyDictName' :: (MonadSupply Int m, MonadThrow m) => Identifier -> m Name
supplyDictName' s = do
  i <- supply
  Identifier s' <- identifyValue s
  return (DictName i s')

supplyTypeName :: (MonadSupply Int m) => Identifier -> m Name
supplyTypeName (Identifier s) = do
  i <- supply
  return (TypeName i s)

supplyTypeVariableName :: (MonadSupply Int m) => Identifier -> m Name
supplyTypeVariableName (Identifier s) = do
  i <- supply
  return (TypeName i (s ++ show i))

supplyClassName :: (MonadSupply Int m) => Identifier -> m Name
supplyClassName (Identifier s) = do
  i <- supply
  return (ClassName i s)

supplyMethodName :: (MonadSupply Int m) => Identifier -> m Name
supplyMethodName (Identifier s) = do
  i <- supply
  return (MethodName i s)
