{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Support for computations which consume values from a (possibly infinite)
-- supply. See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for
-- details.
--
-- Patched to provide MonadCatch/MonadThrow instead of MonadError.
--
module Control.Monad.Supply
( MonadSupply (..)
, SupplyT
, Supply
, evalSupplyT
, evalSupply
, runSupplyT
, runSupply
, supplies
) where

import Control.Monad.Catch
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s
  exhausted :: m Bool

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT (StateT [s] m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix, MonadCatch, MonadThrow)

-- | Supply monad.
newtype Supply s a = Supply (SupplyT s Identity a)
  deriving (Functor, Applicative, Monad, MonadSupply s, MonadFix)

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do (x:xs) <- get
                        put xs
                        return x
  peek = SupplyT $ gets head
  exhausted = SupplyT $ gets null

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

-- | Monoid instance for the supply monad. Actually any monad/monoid pair
-- gives rise to this monoid instance, but we can't write its type like that
-- because it would conflict with existing instances provided by Data.Monoid.
--instance (Monoid a, Monad m) => Monoid (m a) where
instance (Monoid a) => Monoid (Supply s a) where
  mempty = return mempty
  m1 `mappend` m2 = do
    x1 <- m1
    x2 <- m2
    return (x1 `mappend` x2)

-- | Get n supplies.
supplies :: MonadSupply s m => Int -> m [s]
supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) = runIdentity . evalSupplyT s

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a,[s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> (a,[s])
runSupply (Supply s) = runIdentity . runSupplyT s
