module FRP.Rabbit
  ( Reactive(), ReactiveR(..)
  , sync
  , Event(), Behavior()
  , listen, newEvent, never, merge, filterJust
  -- , newBehavior
  , hold, updates, value, {- snapshot, switchE, switch, execute, -} sample
  -- , coalesce, once, split, mergeWith, filterE, gate
  , collectE
  -- , collect, accum
  ) where

import qualified FRP.Rabbit.Internal.Reactive as Reactive
import qualified FRP.Rabbit.Internal.Behavior as Behavior
import qualified FRP.Rabbit.Internal.Event as Event
import qualified FRP.Rabbit.Internal.Sugar as Sugar
import qualified FRP.Rabbit.Internal.Util as Util

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe

type Reactive eff a = Reactive.Reactive eff a
type ReactiveR e a = Reactive (ref :: Ref | e) a

sync :: forall e a. Reactive e a -> Eff e a
sync = Reactive.sync

-- | The `listen` function registers a callback function for an `Event`.
-- |
-- | It's like `.addEventListener()` or subscribe an observable.
-- | `listen` can only subscribe future values to prevent memory leak.
listen :: forall e a. Event e a
          -> (a -> Eff (ref :: Ref | e) Unit)
          -> ReactiveR e (Eff (ref :: Ref | e) Unit)
listen = Event.listen

-- | The `Event eff a` type represents streams of timed values (discrete siganl).
-- | It's a primitive of `FRP.Rabbit`.
-- |
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monoid` and `Functor`.
type Event eff a = Event.Event eff a

-- | The `Behavior eff a` type represents streams of timed values,
-- | but semantically it is continuous time function whose value is the last timed value at the thme.
-- | It's a primitive of `FRP.Rabbit`.
-- |
-- | `Behavior` has current value and an `Event` represents future values.
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monad`.
type Behavior e a = Behavior.Behavior e a

-- | The `newEvent` function create new pair of an `Event` and a push function.
-- | The push function triggers a timed value for the event.
newEvent :: forall e a. ReactiveR e { event :: Event e a, push :: a -> ReactiveR e Unit }
newEvent = Event.newEvent

never :: forall e a. Event e a
never = Event.never

merge :: forall e a. Event e a -> Event e a -> Event e a
merge = Event.merge

filterJust :: forall e a. Event e (Maybe a) -> Event e a
filterJust = Event.filterJust

hold :: forall e a. a -> Event e a -> ReactiveR e (Behavior e a)
hold = Behavior.hold

updates :: forall e a. Behavior e a -> Event e a
updates = Behavior.updates

value :: forall e a. Behavior e a -> Event e a
value = Behavior.value

-- snapshot :: forall e a. (a -> b -> c) -> Event e a -> Behavior e b -> Event e c
-- snapshot = Behavior.snapshot

-- switchE :: forall e a. Behavior e (Event e a) -> Event e a
-- switchE = Behavior.switchE

-- switch :: forall e a. Behavior e (Behavior e a) -> Reactive e (Behavior e a)
-- switch = Behavior.switch

-- execute :: forall e a. Event e (Reactive e a) -> Event e a
-- execute = Event.execute

sample :: forall e a. Behavior e a -> ReactiveR e a
sample = Behavior.sample

-- coalesce :: forall e a. (a -> a -> a) -> Event e a -> Event e a

-- once :: forall e a. Event e a -> Event e a

-- split :: forall e a. Event e [a] -> Event e a

-- mergeWith :: forall e a. (a -> a -> a) -> Event e a -> Event e a -> Event e a

-- filterE :: forall e a. (a -> Bool) -> Event e a -> Event e a

-- gate :: forall e a. Event e a -> Behavior e Bool -> Event e a

collectE :: forall e a b. (a -> b -> b) -> b ->
            Event e a ->
            ReactiveR e (Behavior e b)
collectE = Sugar.collectE

-- collect ::  forall e a b s. (a -> s -> (b, s)) -> s -> Behavior a -> Reactive (Behavior b)

-- accum :: a -> Event (a -> a) -> Reactive (Behavior a)


-- | The `Listener a` type represents callback functions.
-- |
-- | `Listener` is often used by `sinkR` and `sinkE`.
-- | These function are register a callback.
type Listener eff a = a -> Eff (ref :: Ref | eff) Unit
