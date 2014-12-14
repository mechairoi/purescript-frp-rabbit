-- Copyright Josh Bassett
-- Released under the MIT license
-- https://github.com/nullobject/purescript-frp/blob/master/LICENSE.md

module FRP.Rabbit.Signal
  ( stateful
  , runSignal
  , Signal (..)
  , WithRef (..)
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Trans
import Control.Alt
import Control.Plus

-- Represents a signal.
--
-- The type parameter `m` is the underlying monad. The callback function takes
-- a parameter `a`.
newtype Signal m a = Signal ((a -> m Unit) -> m Unit)

type WithRef eff = Eff (ref :: Ref | eff)

instance functorSignal :: (Monad m) => Functor (Signal m) where
  (<$>) f s = Signal (\k -> runSignal s (\a -> k $ f a))

instance applySignal :: (Functor m, Monad m) => Apply (Signal m) where
  (<*>) s t = Signal (\k -> runSignal s $ (\f -> runSignal t (\a -> (k $ f a))))

instance applicativeSignal :: (Functor m, Monad m) => Applicative (Signal m) where
  pure a = Signal (\k -> k a)

instance bindSignal :: (Monad m) => Bind (Signal m) where
  (>>=) m k = Signal (\k' -> runSignal m (\a -> runSignal (k a) k'))

instance monadSignal :: (Monad m) => Monad (Signal m)

instance monadTransSignal :: MonadTrans Signal where
  lift m = Signal (\k -> m >>= k)

instance altSignal :: (Monad m) => Alt (Signal m) where
  (<|>) = merge

instance plusSignal :: (Monad m) => Plus (Signal m) where
  empty = Signal (\k -> return unit)

instance semigroupSignal :: (Monad m) => Semigroup (Signal m a) where
  (<>) = merge

merge :: forall m a. (Monad m) => Signal m a -> Signal m a -> Signal m a
merge a b = Signal (\k -> (runSignal a k) >>= \_ -> runSignal b k)

delay :: forall eff a. a -> Signal (WithRef eff) a -> Signal (WithRef eff) a
delay x0 s = do
  ref <- lift $ newRef x0
  x <- lift $ readRef ref
  x' <- s
  lift $ writeRef ref x'
  return x

-- Creates a new signal that produces difference events between the current and
-- previous value of the given signal.
stateful :: forall eff a b. (a -> b -> b) -> b -> Signal (WithRef eff) a -> Signal (WithRef eff) b
stateful f x0 s = do
  ref <- lift $ newRef x0
  a <- s
  b <- lift $ readRef ref
  let r = f a b
  lift $ writeRef ref r
  return r

callCC :: forall m a b. (Monad m) => ((a -> Signal m b) -> Signal m a) -> Signal m a
callCC f = Signal (\k -> runSignal (f (\a -> Signal (\_ -> k a))) k)

-- Attaches a given callback to a signal.
runSignal :: forall m a. (Monad m) => Signal m a -> (a -> m Unit) -> m Unit
runSignal (Signal sf) f = sf f
