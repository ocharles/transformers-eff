{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect
       ( -- $welcome

         -- * Core API
         Eff, Interpretation(..), handle, LiftProgram(..)) where

import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont

-- | The 'Eff' monad transformer is used to write programs that require access to
-- specific effects. In this library, effects are combined by stacking multiple
-- 'Eff's together. 'Eff's are parameterized by an effect /algebra/. This is a
-- description of programs in a single effect, such as non-determinism (@[]@)or
-- exceptions (@Either e@). As 'Eff' is a monad transformer, @m@ is the monad
-- that 'Eff' transforms, which can itself be another instance of 'Eff'.
newtype Eff effect m a =
  Eff (forall f x. (forall u. (u -> m (f x)) -> effect u -> m (f x)) -> ContT (f x) m a)

-- | In order to run 'Eff' computations, we need to provide a way to run its
-- effects in the underlying monad @m@. We use 'handle' to run a specific
-- 'Interpretation' for an effect. Notice that 'handle' eliminates one layer
-- of 'Eff', returning you with the original @a@ now captured under the
-- result of the effects described by the @effect@ functor.
handle
  :: Monad m
  => Interpretation effect f m -> Eff effect m a -> m (f a)
handle interpreter (Eff m) =
  evalContT (fmap (finalize interpreter) (m (run interpreter)))

-- | Effects can have many different interpretations. We use 'Interpretation' to
-- provide a specific evaluation strategy for an effect. It is parameterized
-- over @effect@, the effect that we are interpreting; and @f@, the target
-- functor that we will evaluate into. Interpretations also have access to the
-- underlying monad @m@ (and are free to incur extra constraints on @m@ if
-- necessary).
data Interpretation effect f m =
  Interpretation {run :: forall a b. (a -> m (f b)) -> effect a -> m (f b) -- ^ 'run' defines how to run programs specified by the @effect@ functor. It is provided both the program to run, but also a continuation for the rest of the program. 'run' is free to invoke this continuation zero, one or many times, in order to build the output of evaluation under some functor @f@.
                 ,finalize :: forall a. a -> f a -- ^ 'finalize' is used to lift the return type of any computation into @f@. It is usually defined as 'return' or 'pure', and plays a similar role.
                 }

-- | 'LiftProgram' defines an @mtl@-style type class for automatically lifting
-- effects into 'Eff' stacks. When exporting libraries that you intend to
-- publish on Hackage, it's suggested that you still provide your own type class
-- (such as 'MonadThrow' or 'MonadHTTP') to avoid locking people into this
-- library, but 'liftProgram' can be useful to define your own instances of
-- that type class for 'Eff'.
class Monad m => LiftProgram f m | m -> f where
  liftProgram :: f a -> m a

instance Monad m => LiftProgram f (Eff f m) where
  liftProgram p = Eff (\run -> ContT (\k -> run k p))

-- TODO Is this sound? Law-abiding? Desirable?
instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, LiftProgram f m) => LiftProgram f (t m) where
  liftProgram = lift . liftProgram

instance MonadTrans (Eff effect) where
  lift m = Eff (\_ -> lift m)

instance Monad m => Functor (Eff effect m) where
  fmap = liftM

instance Monad m => Applicative (Eff effect m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Eff effect m) where
  return a = Eff (\_ -> return a)
  Eff x >>= f = Eff (\i -> x i >>= \a -> case f a of Eff m -> m i)

{- $welcome

Welcome to @effect-interpreters@, a composable approach to managing effects in
functional programming. @effect-interpreters@ provides you with a toolkit to
write programs that are polymorphic over the choice of monad, stipulating that
whatever monad is chosen has access to certain underlying effects.
@effect-interpreters@ comes with the 'Eff' monad to eliminate individual effects
one-by-one, and allows you to easily define your own effects and multiple
interpretations. In this short guide, I'll demonstrate how to get started with
this library.

Within @effect-interpreters@, effects consist of:

1. A language to write programs using operations within the given effect
2. Interpretations of these effects using only the effects of a "smaller" monad.

To start, let's walk through the construction of an effect for failure. You're
probably already familiar with the language to write failing programs - it's the
'Maybe' monad! Within the 'Maybe' monad, we have the ability to fail earlier
by using 'Nothing', or we can produce successful values with 'Just' or 'return'.
Importantly, we can combine multiple 'Maybe' programs together by using its
'Monad' instance.

Now that we have our language, we need to write interpretations of 'Maybe'. One
such interpretation in any bigger monad is to run the program down to @Maybe a@.
That is, we seek a combinator with a type similar to:

@
attempt :: PotentiallyFailing a -> m (Maybe a)
@

Here @attempt@ will attempt to run a program and handle the case when it
attempts to fail. We can build this combinator using @effect-interpreters@ with
'handle':

@
attempt :: Eff Maybe m a -> m (Maybe a)
attempt =
  handle Intepretation { run = \continue p -> case p of
                                                Just a -> continue a
                                                Nothing -> return Nothing
                       , finalize = Just}
@

Let's take this line by line. On the first line with the type. It's similar to
the type I suggested earlier, but to speak specifically about
@PotentiallyFailing@ programs means to be working in an 'Eff' monad transformer
that has the ability to interpret to 'Maybe' programs.

Next, we build our 'Interpretation' and eliminate the 'Maybe' effect by calling
'handle' with this interpretation. An 'Intepreration' consists of a way to run
effectful computations, and a way to lift pure values into the final return
type.

To understand 'run', let's specialize the type of it given what we know. We know
that the 'Intepreration' we are building has type
'Interpretation Maybe Maybe m'. This means that 'run' has the type:

@
run :: forall a b. (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
@

The first argument to 'run' is a /continuation/. Whenever we try and lift a
an effect into 'Eff', we are given the rest of the program - it's then up to the
'Interpretation' if it will actually continue. In the case of failing programs,
it depends. If we're lifting a successful program - that is, @Just a@ - then we
can continue, but if we're lifting a failure then we certainly can't continue.
If you now return to our definition of run, you'll see that we pattern match
on the effectful program that we have to run, continuing or failing as
appropriate.

The other part of an 'Interpretation' is a description of what happens if we
never use the effect that we have access to. That is, what if we are told to
run an 'Eff Maybe' program that never actually uses the ability to fail? In this
case, we have to provide a way to lift pure values into the same context as
'run' - so we simply treat it as success.

-}
