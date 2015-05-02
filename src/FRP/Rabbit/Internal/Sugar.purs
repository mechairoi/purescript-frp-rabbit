module FRP.Rabbit.Internal.Sugar
  ( collectE
  ) where

import Control.Monad.Eff.Ref

import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Reactive

collectE :: forall e a b. (a -> b -> b) -> b ->
            Event e a ->
            ReactiveR e (Behavior e b)
collectE f b0 ea = do
  es <- newEvent
  rb <- b0 `hold` es.event
  ref <-liftR $ newRef b0
  listenTrans ea \a -> do
    b <- liftR $ readRef ref
    let b' = f a b
    liftR $ writeRef ref b'
    es.push b'
    pure unit
  pure rb
