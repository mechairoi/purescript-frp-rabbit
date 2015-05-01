module FRP.Rabbit.Internal.Sugar
  ( stateful
  ) where

import Control.Monad.Eff.Ref

import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Util

stateful :: forall e a b. (a -> b -> b) -> b ->
            Event e a ->
            WithRef e (Behavior e b)
stateful f b0 ea = do
  es <- newEventWithSource
  let rb = b0 `stepperR` es.event
  ref <- newRef b0
  sinkEI (\a -> do b <- readRef ref
                   let b' = f a b
                   writeRef ref b'
                   es.source b'
                   return $ return unit
         ) ea
  return rb
