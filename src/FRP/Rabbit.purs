module FRP.Rabbit
  ( Event(), Behavior()
  , newEvent, listen, never, merge, filterJust

  , newBehavior
  , hold, updates, value, snapshot, switchE, switch, {- execute, -} sample
  , {- coalesce, -} once, {- split, mergeWith, -} gate
  , collectE, collect, accum

  , retain, retainB, cache
  ) where

import FRP.Rabbit.Internal.Reactive(sync)
import qualified FRP.Rabbit.Internal.Behavior as Behavior
import qualified FRP.Rabbit.Internal.Event as Event

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe

-- | The `listen` function registers a callback function for an `Event`.
-- |
-- | It's like `.addEventListener()` or subscribe an observable.
-- | `listen` can only subscribe future values to prevent memory leak.
listen :: forall e a. Event e a
          -> (a -> Eff (ref :: Ref | e) Unit)
          -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
listen ea = sync <<< Event.listen ea

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
newEvent :: forall e a. Eff (ref :: Ref | e) { event :: Event e a
                                             , push :: a -> Eff (ref :: Ref | e) Unit }
newEvent = do
  es <- sync $ Event.newEvent
  pure { event: es.event
       , push: sync <<< es.push }

never :: forall e a. Event e a
never = Event.never

merge :: forall e a. Event e a -> Event e a -> Event e a
merge = Event.merge

filterJust :: forall e a. Event e (Maybe a) -> Event e a
filterJust = Event.filterJust

newBehavior :: forall e a. a ->  Eff (ref :: Ref | e) { behavior :: Behavior e a
                                                      , push :: a -> Eff (ref :: Ref | e) Unit }
newBehavior a = do
  bs <- sync $ Behavior.newBehavior a
  pure { behavior: bs.behavior
       , push: sync <<< bs.push }

hold :: forall e a. a -> Event e a -> Eff (ref :: Ref | e) (Behavior e a)
hold a = sync <<< Behavior.hold a

updates :: forall e a. Behavior e a -> Event e a
updates = Behavior.updates

value :: forall e a. Behavior e a -> Event e a
value = Behavior.value

snapshot :: forall e a b c. (a -> b -> c) -> Event e a -> Behavior e b -> Event e c
snapshot = Behavior.snapshot

switchE :: forall e a. Behavior e (Event e a) -> Event e a
switchE = Behavior.switchE

switch :: forall e a. Behavior e (Behavior e a) -> Eff (ref :: Ref | e) (Behavior e a)
switch = sync <<< Behavior.switch

sample :: forall e a. Behavior e a -> Eff (ref :: Ref | e) a
sample = sync <<< Behavior.sample

once :: forall e a. Event e a -> Event e a
once = Event.once

filterE :: forall e a. (a -> Boolean) -> Event e a -> Event e a
filterE = Event.filterE

gate :: forall e a. Event e a -> Behavior e Boolean -> Event e a
gate = Behavior.gate

collectE :: forall e a b. (a -> b -> b)
         -> b
         -> Event e a
         -> Eff (ref :: Ref | e) (Behavior e b)
collectE f b0 = sync <<< Behavior.collectE f b0

collect :: forall e a s. (a -> s -> s)
        -> s
        -> Behavior e a
        -> Eff (ref :: Ref | e) (Behavior e s)
collect f s0 = sync <<< Behavior.collect f s0

accum :: forall e a. a
      -> Event e (a -> a)
      -> Eff (ref :: Ref | e) (Behavior e a)
accum a0 = sync <<< Behavior.accum a0

retain :: forall e a. Event e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
retain = sync <<< Event.retain

cache :: forall e a. Event e a -> Eff (ref :: Ref | e) (Event e a)
cache = sync <<< Event.cache

retainB :: forall e a. Behavior e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
retainB = sync <<< Behavior.retainB
