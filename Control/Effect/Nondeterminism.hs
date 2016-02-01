{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Nondeterminism where

import Control.Monad (join)
import Control.Effect

-- TODO Can probably generalize over any foldable.

class Monad m => Nondeterministic m where
  liftNondeterminism :: [a] -> m a

instance Monad m => Nondeterministic (Eff [] m) where
  liftNondeterminism = liftProgram

instance {-# OVERLAPPABLE #-} (Nondeterministic m, LiftProgram [] m) => Nondeterministic (Eff f m) where
  liftNondeterminism = liftProgram

choose :: Nondeterministic m => [a] -> m a
choose = liftNondeterminism

runNondeterminism :: Monad m => Eff [] m a -> m [a]
runNondeterminism =
  handle Interpretation {run = concatMapM
                        ,finalize = return}
  where concatMapM f xs = fmap concat (mapM f xs)

-- TODO Non-conflicting names?

mzero :: Nondeterministic m => m a
mzero = choose mempty

mplus :: Nondeterministic m => m a -> m a -> m a
mplus l r = join (choose [l,r])
