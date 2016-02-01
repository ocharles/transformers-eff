{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.State where

import Data.Tuple (swap)
import Control.Effect
import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State, StateT(..), mapStateT)
import Data.Functor.Identity
import qualified Control.Monad.Trans.State.Strict as State

class Monad m => EffState state m | m -> state where
  liftState :: State state a -> m a

instance Monad m => EffState s (Eff (State s) m) where
  liftState = liftProgram
  {-# INLINE liftState #-}

instance (EffState s m, LiftProgram (State s) m) => EffState s (Eff effects m) where
  liftState = liftProgram
  {-# INLINE liftState #-}

runState
  :: Monad m
  => Eff (State s) m a -> s -> m (a,s)
runState eff s =
  fmap swap
       (handle Interpretation {run =
                                 \k p ->
                                   mapStateT (return . runIdentity) p >>= k
                              ,finalize = return
                              ,out = fmap swap . flip runStateT s}
               eff)
{-# INLINE runState #-}

evalState :: Monad m => Eff (State s) m a -> s -> m a
evalState m s = fmap fst (runState m s)
{-# INLINE evalState #-}

execState :: Monad m => Eff (State s) m a -> s -> m s
execState m s = fmap snd (runState m s)
{-# INLINE execState #-}

get :: (EffState state m) => m state
get = liftState (State.get)
{-# INLINE get #-}

put :: (EffState state m) => state -> m ()
put x = liftState (State.put x)
{-# INLINE put #-}

modify :: (EffState state m) => (state -> state) -> m ()
modify f = liftState (State.modify f)
{-# INLINE modify #-}

modify' :: (EffState state m) => (state -> state) -> m ()
modify' f = liftState (State.modify' f)
{-# INLINE modify' #-}

gets :: (EffState state m) => (state -> state) -> m state
gets f = liftState (State.gets f)
{-# INLINE gets #-}
