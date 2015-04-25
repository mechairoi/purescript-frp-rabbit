module FRP.Rabbit
  ( Sink(), WithRef()
  , Event(), sinkE, newEventWithSource
  , Reactive(), sinkR, stepperR, switcherR
  , stateful
  ) where

import qualified FRP.Rabbit.Internal.Reactive as Reactive
import qualified FRP.Rabbit.Internal.Event as Event
import qualified FRP.Rabbit.Internal.Sugar as Sugar
import qualified FRP.Rabbit.Internal.Util as Util

import Control.Monad.Eff
import Control.Monad.Eff.Ref
type WithRef eff a = Eff (ref :: Ref | eff) a

-- | The `Sink a` type represents callback functions.
-- |
-- | `Sink` is often used by `sinkR` and `sinkE`.
-- | These function are register a callback.
type Sink eff a = a -> WithRef eff Unit

-- | The `Event eff a` type represents streams of timed values (discrete siganl).
-- | It's a primitive of `FRP.Rabbit`.
-- |
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monoid` and `Functor`.
type Event eff a = Event.Event eff a

-- | The `newEventWithSource` function create new pair of an `Event` and a source.
-- | The source function triggers a timed value for the event.
newEventWithSource :: forall e a. WithRef e { event :: Event e a, source :: Sink e a }
newEventWithSource = Event.newEventWithSource

-- | The `sinkE` function registers a callback function for an `Event`.
-- |
-- | It's like `.addEventListener()` or subscribe an observable.
-- | `sinkE` can only subscribe future values to prevent memory leak.
-- | Please use `sinkR` to subscribe an `Reactive`.
sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
sinkE = Event.sinkE


-- | The `Reactive eff a` type represents streams of timed values,
-- | but semantically it is continuous time function whose value is the last timed value at the thme.
-- | It's a primitive of `FRP.Rabbit`.
-- |
-- | `Reactive` has current value and an `Event` represents future values.
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monad`.
type Reactive e a = Reactive.Reactive e a

-- | The `sinkR` function registers a callback function for a `Reactive`.
-- |
-- | It's like `.addEventListener()` or subscribe an observable.
-- | `sinkR` can only subscribe current and future values to prevent memory leak.
-- | Please use `sinkE` to subscribe an `Event`.
sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
sinkR = Reactive.sinkR

stepperR :: forall a e. a -> Event e a -> Reactive e a
stepperR = Reactive.stepperR

switcherR :: forall a e. Reactive e a -> Event e (Reactive e a) -> Reactive e a
switcherR = Reactive.switcherR

stateful :: forall e a b. (a -> b -> b) -> b ->
            Event e a ->
            WithRef e (Reactive e b)
stateful = Sugar.stateful
