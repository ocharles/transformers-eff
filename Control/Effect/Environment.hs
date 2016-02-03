{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Environment where

import Control.Effect
import Control.Monad.Morph (lift, hoist, generalize)
import Control.Monad.Trans.Reader (Reader, ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader

class Monad m => EffEnvironment env m | m -> env where
  liftEnvironment :: Reader env a -> m a

instance Monad m => EffEnvironment env (Eff (Reader env) m) where
  liftEnvironment = interpret
  {-# INLINE liftEnvironment #-}

instance {-# OVERLAPPABLE #-} (EffEnvironment env m) => EffEnvironment env (Eff effects m) where
  liftEnvironment = lift . liftEnvironment
  {-# INLINE liftEnvironment #-}

ask :: (EffEnvironment env m) => m env
ask = liftEnvironment Reader.ask
{-# INLINE ask #-}

asks :: (EffEnvironment a m) => (a -> b) -> m b
asks f = fmap f ask
{-# INLINE asks #-}

runInEnvironment
  :: Monad m
  => Eff (Reader env) m a -> env -> m a
runInEnvironment eff env =
  runReaderT (translate (lift . hoist generalize) eff)
             env
{-# INLINE runInEnvironment #-}

mapEnvironment
  :: (EffEnvironment env m)
  => (env -> env') -> Eff (Reader env') m a -> m a
mapEnvironment f m = ask >>= runInEnvironment m . f
{-# INLINE mapEnvironment #-}
