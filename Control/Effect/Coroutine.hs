{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Effect.Coroutine where

import Control.Applicative
import Data.Functor.Identity
import Control.Effect
import Control.Monad.Free.Church
import Control.Monad.Trans.Cont

data Yield a v = Yield a v
  deriving (Functor)

data Coroutine m a
  = Done
  | Step a
         (m (Coroutine m a))

yield :: LiftProgram (Yield a) m => a -> m ()
yield x = liftProgram (Yield x ())

forkCoroutine
  :: Monad m
  => Eff (Yield a) m b -> m (Coroutine m a)
forkCoroutine =
  fmap getConst .
  handle Interpretation {run =
                           \k (Yield y v) ->
                             ContT (return . Step y . runContT (k v))
                        ,finalize = return
                        ,out =
                           \m -> fmap Const (runContT m (\_ -> return Done))}
