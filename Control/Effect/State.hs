{-# LANGUAGE FlexibleContexts #-}

module Control.Effect.State where

import Control.Effect
import Control.Monad (join)
import Control.Monad.Trans.State.Strict (State, StateT(..), mapStateT)
import Data.Functor.Identity
import qualified Control.Monad.Trans.State.Strict as State

runState :: Monad m
         => Eff (State s) m a -> s -> m (a,s)
runState eff =
  runStateT (run (\k m -> mapStateT (return . runIdentity) m >>= k)
                 (\m -> StateT (\s -> join (fmap (flip runStateT s) m)))
                 eff)
{-# INLINE runState #-}

evalState :: Monad m => Eff (State s) m a -> s -> m a
evalState m s = fmap fst (runState m s)
{-# INLINE evalState #-}

execState :: Monad m => Eff (State s) m a -> s -> m s
execState m s = fmap snd (runState m s)
{-# INLINE execState #-}

get :: (Interprets (State state) m) => m state
get = interpret (State.get)
{-# INLINE get #-}

put :: (Interprets (State state) m) => state -> m ()
put x = interpret (State.put x)
{-# INLINE put #-}

modify :: (Interprets (State state) m) => (state -> state) -> m ()
modify f = interpret (State.modify f)
{-# INLINE modify #-}

modify' :: (Interprets (State state) m) => (state -> state) -> m ()
modify' f = interpret (State.modify' f)
{-# INLINE modify' #-}

gets :: (Interprets (State state) m) => (state -> state) -> m state
gets f = interpret (State.gets f)
{-# INLINE gets #-}
