{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Effect.IO (UIO, runUIO, syncIO, EffUIO(..), runExceptionalIO) where

import Control.Effect
import Control.Exception (SomeException, throwIO, try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Effect.Exception as Ex

-- | Lift an 'IO' action and explictly throw an synchronous IO exceptions that
-- occur.
syncIO :: (Interprets (Either SomeException) m,EffUIO m)
       => IO a -> m a
syncIO io = liftUIO (UIO (try io)) >>= interpret -- TODO This is catching async

runExceptionalIO
  :: Eff (Either SomeException) IO a -> IO a
runExceptionalIO = Ex.try >=> either throwIO return

newtype UIO a =
  UIO {runUIO :: IO a}
  deriving (Functor,Applicative,Monad)

class Monad m => EffUIO m where
  liftUIO :: UIO a -> m a

instance EffUIO IO where
  liftUIO (UIO io) = io

instance EffUIO UIO where
  liftUIO = id

instance EffUIO m => EffUIO (Eff r m) where
  liftUIO = lift . liftUIO
