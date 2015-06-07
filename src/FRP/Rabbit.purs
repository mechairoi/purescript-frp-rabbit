module FRP.Rabbit
  ( Event(), Behavior()
  , newEvent, newBehavior
  , listen

  , never, merge, filterJust , hold, updates, value, snapshot, switchE, switch
  {- , execute -}, sample {- , coalesce -}, once {- , split -}

  {- mergeWith -}, filterE, gate, collectE, collect, accum

  , executeEff

  , retain, retainB, cache
  ) where

import FRP.Rabbit.Class
import FRP.Rabbit.Internal.Reactive(sync)
import qualified FRP.Rabbit.Internal.Behavior as Behavior
import qualified FRP.Rabbit.Internal.Event as Event
import qualified FRP.Rabbit.Internal.Eff as Eff

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Bind (join)
import Data.Maybe

-- | A stream of events (discrete siganl).
-- |
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monoid` and `Functor`.
type Event eff a = Event.Event eff a

-- | A s time-varying value.
-- | It semantically represents continuous time function whose value is the last timed value at the time.
-- |
-- | `Behavior` has current value and an `Event` represents future values.
-- | These timed values are inhabitants of _a_.
-- | `Event` is an instance of `Monad`.
type Behavior e a = Behavior.Behavior e a

-- | Returns an event, and a push action for pushing a value into the event.
newEvent :: forall e a. Eff (ref :: Ref | e) { event :: Event e a
                                             , push :: a -> Eff (ref :: Ref | e) Unit }
newEvent = do
  es <- sync $ Event.newEvent
  pure { event: es.event
       , push: sync <<< es.push }

-- | Create a new Behavior along with an action to push changes into it.
-- | The returning behavior is recommended to `retainB`.
newBehavior :: forall e a. a ->  Eff (ref :: Ref | e) { behavior :: Behavior e a
                                                      , push :: a -> Eff (ref :: Ref | e) Unit }
newBehavior a = do
  bs <- sync $ Behavior.newBehavior a
  pure { behavior: bs.behavior
       , push: sync <<< bs.push }

-- | Listen for firings of this event. The returned `Eff _ Unit` is an Eff action that unregisters the listener. This is the observer pattern.
-- | To listen to a Behavior use `listen (value b)` handler or `listen (updates b)` handler
-- | `listen` function can subscribe only future values to prevent memory leak.
listen :: forall e a. Event e a
          -> (a -> Eff (ref :: Ref | e) Unit)
          -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
listen ea = sync <<< Event.listen ea

-- | An event that never fires.
never :: forall e a. Event e a
never = Event.never

-- | Merge two streams of events of the same type.

-- | In the case where two event occurrences are simultaneous, both will be delivered.
merge :: forall e a. Event e a -> Event e a -> Event e a
merge = Event.merge

-- | Unwrap Just values, and discard event occurrences with Nothing values.
filterJust :: forall e a. Event e (Maybe a) -> Event e a
filterJust = Event.filterJust

-- | Create a behavior with the specified initial value, that gets
-- | updated by the values coming through the event.
hold :: forall e a. a -> Event e a -> Eff (ref :: Ref | e) (Behavior e a)
hold a = sync <<< Behavior.hold a

-- | An event that gives the updates for the behavior.
updates :: forall e a. Behavior e a -> Event e a
updates = Behavior.updates

-- | An event that gives the current value and the updates for the behavior.
value :: forall e a. Behavior e a -> Event e a
value = Behavior.value

-- | Sample the behavior at the time of the event firing.
snapshot :: forall e a b c. (a -> b -> c) -> Event e a -> Behavior e b -> Event e c
snapshot = Behavior.snapshot

-- | Unwrap an event inside a behavior to give a time-varying event implementation.
switchE :: forall e a. Behavior e (Event e a) -> Event e a
switchE = Behavior.switchE

-- | Unwrap a behavior inside another behavior to give a time-varying behavior implementation.
switch :: forall e a. Behavior e (Behavior e a) -> Eff (ref :: Ref | e) (Behavior e a)
switch = pure <<< join

-- | Obtain the current value of a behavior.
sample :: forall e a. Behavior e a -> Eff (ref :: Ref | e) a
sample = sync <<< Behavior.sample

-- | Throw away all event occurrences except for the first one.
once :: forall e a. Event e a -> Event e a
once = Event.once

-- | Only keep event occurrences for which the predicate is true.
filterE :: forall e a. (a -> Boolean) -> Event e a -> Event e a
filterE = Event.filterE

-- | Let event occurrences through only when the behavior's value is True.
gate :: forall e a. Event e a -> Behavior e Boolean -> Event e a
gate = Behavior.gate

-- | Transform an event with a generalized state loop (a mealy machine). The function is passed the input and the old state and returns the new state and output value
-- | The returning behavior is recommended to `retainB`.
collectE :: forall e a b. (a -> b -> b)
         -> b
         -> Event e a
         -> Eff (ref :: Ref | e) (Behavior e b)
collectE f b0 = sync <<< Behavior.collectE f b0

-- Transform a behavior with a generalized state loop (a mealy machine). The function is passed the input and the old state and returns the new state and output value.
-- | The returning behavior is recommended to `retainB`.
collect :: forall e a s. (a -> s -> s)
        -> s
        -> Behavior e a
        -> Eff (ref :: Ref | e) (Behavior e s)
collect f s0 = sync <<< Behavior.collect f s0

-- | Accumulate state changes given in the input event.
-- | The returning behavior is recommended to `retainB`.
accum :: forall e a. a
      -> Event e (a -> a)
      -> Eff (ref :: Ref | e) (Behavior e a)
accum a0 = sync <<< Behavior.accum a0

-- | Execute the specified IO operation synchronously and fire the output event.
executeEff :: forall e a. Event e (Eff (ref :: Ref | e) a)
           -> Eff (ref :: Ref | e) (Event e a)
executeEff = sync <<< Eff.executeEff

-- | Keep the event as active.
-- | The `retain` function returns the function to release.
-- |
-- | JavaScript has no weak reference. So we have to manually manage activity
-- | of `Event`s. To prevent memory leak, `Event`s are activated only if
-- | one or more listeners are exist (like reference counting). This function
-- | simply registers a dummy no-op lisetener.
retain :: forall e a. Event e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
retain = sync <<< Event.retain

-- | Cache the event occurrences and fire listeners.
-- | The returning event is recommended to `retain`.
cache :: forall e a. Event e a -> Eff (ref :: Ref | e) (Event e a)
cache = sync <<< Event.cache

-- | Keep the `Behavior` as active.
-- | The `retainB` function returns the function to release.
-- |
-- | Same as `retain` except for Behaviors.
retainB :: forall e a. Behavior e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
retainB = sync <<< Behavior.retainB
