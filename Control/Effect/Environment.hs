{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Environment where

import Control.Effect
import Data.Functor.Identity

class Monad m => EffEnvironment env m | m -> env where
  liftEnvironment :: (env -> a) -> m a

instance Monad m => EffEnvironment env (Eff ((->) env) m) where
  liftEnvironment = liftProgram

ask :: (EffEnvironment env m) => m env
ask = liftEnvironment id

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
