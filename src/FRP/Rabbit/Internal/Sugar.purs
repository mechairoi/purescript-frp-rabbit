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
            Reactive e (Behavior e b)
collectE f b0 ea = Reactive $ do
  es <- sync $ newEvent
  rb <- sync $ b0 `hold` es.event
  ref <- newRef b0
  sync $ listenI ea (\a -> do b <- readRef ref
                              let b' = f a b
                              writeRef ref b'
                              sync $ es.push b'
                              pure $ pure unit
                    )
  pure rb
