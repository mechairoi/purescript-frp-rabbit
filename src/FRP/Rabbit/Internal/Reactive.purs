module FRP.Rabbit.Internal.Reactive
  ( Reactive(..)
  , ReactiveR(..)
  , sync
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref (Ref())
import Control.Monad.Eff.Class

newtype Reactive e a = Reactive (Eff e { r :: a, after :: Eff e Unit } )

type ReactiveR e a = Reactive (ref :: Ref | e) a

unReactive :: forall e a. Reactive e a
           -> Eff e { r :: a, after :: Eff e Unit }
unReactive (Reactive r) = r

sync :: forall e a. Reactive e a -> Eff e a
sync (Reactive eff) = do
  r <- eff
  r.after
  pure r.r

instance functorReactive :: Functor (Reactive e) where
  (<$>) g (Reactive fa) = Reactive $ do
    a <- fa
    pure { r: g a.r, after: a.after }

instance applicativeReactive :: Applicative (Reactive e) where
  pure a = Reactive $ pure { r: a, after: pure unit }

instance applyReactive :: Apply (Reactive e) where
  (<*>) (Reactive uf) (Reactive ua) = Reactive $ do
    rf <- uf
    ra <- ua
    pure { r: rf.r ra.r
         , after: do
             rf.after
             ra.after }

instance bindReactive :: Bind (Reactive e) where
  (>>=) (Reactive ma) k = Reactive $ do
    ra <- ma
    rb <- unReactive $ k ra.r
    pure { r: rb.r
         , after: do
             ra.after
             rb.after }

instance monadReactive :: Monad (Reactive e)

instance monadEffReactive :: MonadEff e (Reactive e) where
  liftEff eff = Reactive $ do
    a <- eff
    pure { r: a, after: pure unit }
