{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Effect.IO (UIO, runUIO, syncIO, MonadUIO(..), runExceptionalIO) where

import Control.Effect
import Control.Effect.Exception hiding (try)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import qualified Control.Effect.Exception as Ex

-- | Lift an 'IO' action and explictly throw an synchronous IO exceptions that
-- occur.
syncIO :: (EffException SomeException m, MonadUIO m) => IO a -> m a
syncIO io = liftUIO (UIO (try io)) >>= liftEither -- TODO This is catching async

runExceptionalIO :: Eff (Either SomeException) IO a -> IO a
runExceptionalIO = Ex.try >=> either throwIO return

class Monad m => MonadUIO m where
  liftUIO :: UIO a -> m a

newtype UIO a =
  UIO {runUIO :: IO a}
  deriving (Functor,Applicative,Monad)

instance MonadUIO IO where
  liftUIO (UIO io) = io

instance MonadUIO UIO where
  liftUIO = id

instance MonadUIO m => MonadUIO (Eff r m) where
  liftUIO = lift . liftUIO
