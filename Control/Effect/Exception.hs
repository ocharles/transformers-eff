{-# LANGUAGE FunctionalDependencies #-}

module Control.Effect.Exception where

class EffException e m | m -> e where
  liftException :: Either e a -> m a
