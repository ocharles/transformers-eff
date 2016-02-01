{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Environment where

import Control.Effect
import Data.Functor.Identity

class Monad m => EffEnvironment env m | m -> env where
  liftEnvironment :: (env -> a) -> m a

instance Monad m => EffEnvironment env (Eff ((->) env) m) where
  liftEnvironment = liftProgram

instance (EffEnvironment env m, LiftProgram ((->) env) m) => EffEnvironment env (Eff effects m) where
  liftEnvironment = liftProgram

ask :: (EffEnvironment env m) => m env
ask = liftEnvironment id

asks :: (EffEnvironment a m) => (a -> b) -> m b
asks f = fmap f ask

runInEnvironment
  :: Monad m
  => Eff ((->) env) m a -> env -> m a
runInEnvironment eff env =
  fmap runIdentity
       (handle Interpretation {run =
                                 \k p ->
                                   k (p env)
                              ,finalize = return}
               eff)

mapEnvironment
  :: (EffEnvironment env m)
  => (env -> env') -> Eff ((->) env') m a -> m a
mapEnvironment f =
  fmap runIdentity .
  handle Interpretation {run = \k p -> ask >>= k . p . f
                        ,finalize = return}
