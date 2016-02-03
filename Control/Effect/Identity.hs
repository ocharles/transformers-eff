{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Identity where

import Control.Effect
import qualified Data.Functor.Identity as Id
import qualified Control.Monad.Trans.Identity as Id

runIdentity
  :: Monad m
  => Eff Id.Identity m a -> m a
runIdentity = Id.runIdentityT . translate (return . Id.runIdentity)
{-# INLINE runIdentity #-}
