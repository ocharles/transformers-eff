{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Nondeterminism where

import Control.Monad (join)
import Control.Effect
import qualified Pipes as P
import qualified Pipes.Prelude as P

-- TODO Can probably generalize over any foldable.

class Monad m => Nondeterministic m where
  liftNondeterminism :: [a] -> m a

instance Monad m => Nondeterministic (Eff [] m) where
  liftNondeterminism = liftProgram
  {-# INLINE liftNondeterminism #-}

instance {-# OVERLAPPABLE #-} (Nondeterministic m, LiftProgram [] m) => Nondeterministic (Eff f m) where
  liftNondeterminism = liftProgram
  {-# INLINE liftNondeterminism #-}

choose :: Nondeterministic m => [a] -> m a
choose = liftNondeterminism
{-# INLINE choose #-}

runNondeterminism :: Monad m => Eff [] m a -> m [a]
runNondeterminism =
  handle Interpretation {run =
                           \k choices ->
                             P.Select (P.for (P.each choices)
                                             (P.enumerate . k))
                        ,finalize = return
                        ,out = P.toListM . P.enumerate}
{-# INLINE runNondeterminism #-}

-- TODO Non-conflicting names?

mzero :: Nondeterministic m => m a
mzero = choose mempty

mplus :: Nondeterministic m => m a -> m a -> m a
mplus l r = join (choose [l,r])
