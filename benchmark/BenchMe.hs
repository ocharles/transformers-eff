{-# LANGUAGE FlexibleContexts #-}

module BenchMe (right1, right2) where

import Control.Effect
import Control.Effect.Nondeterminism
import Control.Effect.Exception
import Data.Functor.Identity

newtype TooBig = TooBig Integer deriving (Show)

example :: (EffException TooBig m)
        => m Integer -> m Integer
example m =
  do v <- m
     if v > 5
        then throw (TooBig v)
        else return v

handle_
  :: (Monad m,EffException TooBig m)
  => Eff (Either TooBig) m Integer -> m Integer
handle_ m =
  do res <- try m
     case res of
       Left e ->
         case e of
           TooBig v
             | v <= 7 -> return v
           _ -> throw e
       Right a -> return a

right1 :: [Integer] -> Either TooBig [Integer]
right1 = runIdentity . try . runNondeterminism . handle_ . example . choose

right2 :: [Integer] -> Either TooBig [Integer]
right2 = runIdentity . try . runNondeterminism . handle_ . example . choose
