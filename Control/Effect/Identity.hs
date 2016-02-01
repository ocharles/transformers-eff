{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Identity where

import Control.Monad
import Control.Effect
import qualified Data.Functor.Identity as Id
import qualified Control.Monad.Trans.Identity as Id
import qualified Control.Monad.Trans.State.Strict as State

runIdentity
  :: Monad m
  => Eff Id.Identity m a -> m a
runIdentity =
  fmap Id.runIdentity .
  handle Interpretation {run = \k -> k . Id.runIdentity
                        ,finalize = return
                        ,out = fmap Id.Identity . Id.runIdentityT}
{-# INLINE runIdentity #-}
