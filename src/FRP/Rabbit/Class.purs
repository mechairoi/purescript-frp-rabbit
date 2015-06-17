module FRP.Rabbit.Class where

import Control.Monad.Eff.Ref
import Data.Monoid

import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Reactive
import FRP.Rabbit.Internal.Util
import Control.Bind (join)

instance monoidEvent :: Monoid (Event e a) where
  mempty = never

instance functorEvent :: Functor (Event e) where
  (<$>) = apE

instance semigroupEvent :: Semigroup (Event e a) where
  (<>) = merge


instance functorBehavior :: Functor (Behavior e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeBehavior :: Applicative (Behavior e) where
  pure a = Behavior { last: unsafePerformEff $ newRef a
                    , updates: (mempty :: Event e _)
                    , value_: (mempty :: Event e _)
                    }

instance applyBehavior :: Apply (Behavior e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindBehavior :: Bind (Behavior e) where
  (>>=) ba k = unsafePerformEff $ sync do
    a0 <- sample ba
    let bb0 = k a0
    b0 <- sample $ bb0
    b0 `hold` (Event \listener -> do
                    unlistenerB <- listenTrans (value_ $ bb0) listener
                    unlistenerRef <- liftR $ newRef unlistenerB
                    unlistenerA <- listenTrans (value_ ba) (\a -> do
                      unlistenerB <- listenTrans (value $ k a) listener
                      liftR $ join $ readRef unlistenerRef
                      liftR $ writeRef unlistenerRef $ unlistenerB
                      pure unit)
                    pure do
                      join $ readRef unlistenerRef
                      unlistenerA)

instance monadBehavior :: Monad (Behavior e)
