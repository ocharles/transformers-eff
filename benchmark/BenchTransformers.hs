{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BenchTransformers (right1, right2) where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import qualified Pipes as P
import qualified Pipes.Prelude as P

class Monad m => MonadThrow e m | m -> e where
  throw :: e -> m a

instance Monad m => MonadThrow e (ExceptT e m) where
  throw = ExceptT . return . Left

instance MonadThrow e m => MonadThrow e (P.ListT m) where
  throw = lift . throw

class Monad m => MonadChoose m where
  choose :: [a] -> m a

instance Monad m => MonadChoose (P.ListT m) where
  choose = P.Select . P.each

instance MonadChoose m => MonadChoose (ExceptT e m) where
  choose = lift . choose

newtype TooBig = TooBig Integer deriving (Show)

example :: (MonadThrow TooBig m)
        => m Integer -> m Integer
example m =
  do v <- m
     if v > 5
        then throw (TooBig v)
        else return v

handle_ :: (MonadThrow TooBig m)
        => ExceptT TooBig m Integer -> m Integer
handle_ (ExceptT m) =
  do res <- m
     case res of
       Left e ->
         case e of
           TooBig v
             | v <= 7 -> return v
           _ -> throw e
       Right a -> return a

runNondeterminism :: Monad m => P.ListT m a -> m [a]
runNondeterminism = P.toListM . P.enumerate

right1 :: [Integer] -> Either TooBig [Integer]
right1 = runIdentity . runExceptT . runNondeterminism . handle_ . example . choose

right2 :: [Integer] -> Either TooBig [Integer]
right2 = runIdentity . runExceptT . runNondeterminism . handle_ . example . choose
