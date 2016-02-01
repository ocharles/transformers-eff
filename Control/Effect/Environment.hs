{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Environment where

import Control.Effect
import Data.Functor.Identity
import Control.Monad.Trans.Reader (ReaderT(..))

class Monad m => EffEnvironment env m | m -> env where
  liftEnvironment :: (env -> a) -> m a

instance Monad m => EffEnvironment env (Eff ((->) env) m) where
  liftEnvironment = liftProgram
  {-# INLINE liftEnvironment #-}

instance {-# OVERLAPPABLE #-} (EffEnvironment env m, LiftProgram ((->) env) m) => EffEnvironment env (Eff effects m) where
  liftEnvironment = liftProgram
  {-# INLINE liftEnvironment #-}

ask :: (EffEnvironment env m) => m env
ask = liftEnvironment id
{-# INLINE ask #-}

asks :: (EffEnvironment a m) => (a -> b) -> m b
asks f = fmap f ask
{-# INLINE asks #-}

runInEnvironment
  :: Monad m
  => Eff ((->) env) m a -> env -> m a
runInEnvironment eff env =
  fmap runIdentity
       (handle Interpretation {run = \k p -> ReaderT (\e -> return e) >>= k . p
                              ,finalize = return
                              ,out = \a -> fmap Identity (runReaderT a env)}
               eff)
{-# INLINE runInEnvironment #-}

mapEnvironment
  :: (EffEnvironment env m)
  => (env -> env') -> Eff ((->) env') m a -> m a
mapEnvironment f m = ask >>= runInEnvironment m . f
{-# INLINE mapEnvironment #-}
