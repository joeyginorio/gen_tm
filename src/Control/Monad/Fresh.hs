{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Fresh where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.Trans (MonadTrans (..))
import Hedgehog (MonadGen (..), distributeT)

newtype FreshT m a = FreshT {unFreshT :: (StateT Int m) a}
  deriving stock (Functor)

runFreshT :: forall m a. Monad m => FreshT m a -> m a
runFreshT = flip evalStateT 0 . unFreshT

instance Monad m => Monad (FreshT m) where
  return = FreshT . return
  (FreshT m) >>= f = FreshT $ m >>= unFreshT . f

instance (Functor f, Monad f) => Applicative (FreshT f) where
  pure = FreshT . pure
  (FreshT f) <*> (FreshT a) = FreshT $ f <*> a

instance (Monad m, Functor m, MonadPlus m) => Alternative (FreshT m) where
  empty = mzero
  (<|>) = mplus

instance MonadPlus m => MonadPlus (FreshT m) where
  mzero = FreshT mzero
  mplus (FreshT m) (FreshT m') = FreshT $ mplus m m'

instance MFunctor FreshT where
  hoist nat m = FreshT $ hoist nat (unFreshT m)

instance MonadTrans FreshT where
  lift = FreshT . lift

instance MonadGen m => MonadGen (FreshT m) where
  type GenBase (FreshT m) = FreshT (GenBase m)
  toGenT = hoist FreshT . distributeT . unFreshT . hoist toGenT
  fromGenT = hoist fromGenT . distributeT
