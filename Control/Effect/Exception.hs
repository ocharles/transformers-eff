{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Exception where

import Control.Effect
import Control.Monad ((>=>))
import Control.Monad.Trans.Except

class Monad m => EffException e m | m -> e where
  liftEither :: Either e a -> m a

instance Monad m => EffException e (Eff (Either e) m) where
  liftEither = liftProgram
  {-# INLINE liftEither #-}

instance {-# OVERLAPPABLE #-} (EffException e m, LiftProgram (Either e) m) => EffException e (Eff f m) where
  liftEither = liftProgram
  {-# INLINE liftEither #-}

throw :: EffException e m => e -> m a
throw = liftEither . Left
{-# INLINE throw #-}

try :: Monad m => Eff (Either e) m a -> m (Either e a)
try =
  handle Interpretation {run = \k -> ExceptT . return >=> k
                        ,finalize = return
                        ,out = runExceptT}
{-# INLINE try #-}
