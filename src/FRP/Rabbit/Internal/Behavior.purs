module FRP.Rabbit.Internal.Behavior
  ( Behavior(..)
  , newBehavior

  , hold
  , updates
  , value
  , snapshot
  , switchE
  , sample

  , gate
  , collectE
  , collect
  , accum

  , retainB
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Bind (join)
import Data.Monoid
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

newtype Behavior e a = Behavior { last :: (RefVal a)
                                , updates :: (Event e a)
                                , value_ :: (Event e a) }

newBehavior :: forall e a. a -> ReactiveR e { behavior :: Behavior e a
                                            , push :: a -> ReactiveR e Unit }
newBehavior a = do es <- newEvent
                   behavior <- a `hold` es.event
                   pure { behavior: behavior, push: es.push }

hold :: forall a e. a -> Event e a -> ReactiveR e (Behavior e a)
hold a0 e = do
  last <- liftR $ newRef a0
  es <- newEventI(\push -> listenTrans e \a -> do
                     liftR $ writeRef last a
                     push a)
  pure $ Behavior { last: last
                  , updates: e
                  , value_: es.event }

updates :: forall e a. Behavior e a -> Event e a
updates (Behavior b) = b.updates

value_ :: forall e a. Behavior e a -> Event e a
value_ (Behavior b) = b.value_

value :: forall e a. Behavior e a -> Event e a
value ba = Event \listener -> do
  a0 <- sample ba
  listener a0
  listenTrans (value_ ba) listener

snapshot :: forall e a b c. (a -> b -> c)
            -> Event e a
            -> Behavior e b
            -> Event e c
snapshot f ea bb = Event \l -> do
  release <- retainB bb
  unlisten <- listenTrans ea (\a -> do
    b <- sample bb
    l $ f a b
  )
  return do release
            unlisten

switchE :: forall e a. Behavior e (Event e a) -> Event e a
switchE bea = Event \l -> do
  unlistenRef <- liftR $ newRef $ pure unit
  unlistenB <- listenTrans (value bea) \ea -> do
    liftR $ join $ readRef unlistenRef
    unlisten <- listenTrans ea l
    liftR $ writeRef unlistenRef $ unlisten
  pure do
    join $ readRef unlistenRef
    unlistenB

sample :: forall e a. Behavior e a -> ReactiveR e a
sample (Behavior ba) = liftR $ readRef ba.last

gate :: forall e a. Event e a -> Behavior e Boolean -> Event e a
gate ea bb = Event \l -> do
  unlistenRef <- liftR $ newRef $ pure unit
  unlistenB <- listenTrans (value bb) \b -> do
    liftR $ join $ readRef unlistenRef
    unlisten <- if b then listenTrans ea l else pure (pure unit)
    liftR $ writeRef unlistenRef unlisten
  pure do
    join $ readRef unlistenRef
    unlistenB

collectE :: forall e a s. (a -> s -> s)
            -> s
            -> Event e a
            -> ReactiveR e (Behavior e s)
collectE f s0 ea = do
  sRef <- liftR $ newRef s0
  es <- newEventI \push ->
    listenTrans ea \a -> do
      s <- liftR $ readRef sRef
      let s' = f a s
      liftR $ writeRef sRef s'
      push s'
  bs <- s0 `hold` es.event
  pure bs

collect :: forall e a s. (a -> s -> s)
           -> s
           -> Behavior e a
           -> ReactiveR e (Behavior e s)
collect f s0 ba = do
  a0 <- sample ba
  let s1 = (f a0 s0)
  sRef <- liftR $ newRef s1
  es <- newEventI \push ->
    listenTrans (updates ba) \a -> do
      s <- liftR $ readRef sRef
      let s' = f a s
      liftR $ writeRef sRef s'
      push s'
  s1 `hold` es.event

accum :: forall e a. a -> Event e (a -> a) -> ReactiveR e (Behavior e a)
accum a0 ef = collectE (\f a -> f a) a0 ef

retainB :: forall e a. Behavior e a -> ReactiveR e (Eff (ref :: Ref | e) Unit)
retainB b = retain $ value_ b

-- switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
-- switcherR r er = join (r `hold` er)
