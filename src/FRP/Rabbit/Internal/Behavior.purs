module FRP.Rabbit.Internal.Behavior
  ( Behavior()
  , newBehavior

  , hold
  , updates
  , value
  , snapshot
  , switchE
  , switch
  , sample

  , gate
  , collectE
  , collect
  , accum

  , retainB
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Cont.Trans
import Control.Bind (join)
import Data.Monoid
import Data.Maybe
import Data.Int
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

newtype Behavior e a = Behavior { last :: (RefVal a)
                                , event :: (Event e a)
                                , listenCounter :: (RefVal Int)
                                , deactivate :: RefVal (Eff (ref :: Ref | e) Unit) }

instance functorBehavior :: Functor (Behavior e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeBehavior :: Applicative (Behavior e) where
  pure a = Behavior { last: unsafePerformEff $ newRef a
                    , event: (mempty :: Event e _)
                    , listenCounter: unsafePerformEff $ newRef zero
                    , deactivate: unsafePerformEff $ newRef $ pure unit
                    }

instance applyBehavior :: Apply (Behavior e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindBehavior :: Bind (Behavior e) where
  (>>=) ba k = unsafePerformEff do
    a0 <- sync $ sample ba
    let bb0 = k a0
    b0 <- sync $ sample $ bb0
    pure $ b0 `stepperR` (Event \listener -> do
                    unlistenerB <- listenTrans (updates $ bb0) listener
                    unlistenerRef <- liftR $ newRef unlistenerB
                    unlistenerA <- listenTrans (updates ba) (\a -> do
                      liftR $ join $ readRef unlistenerRef
                      unlistenerB <- listenTrans (value $ k a) listener
                      liftR $ writeRef unlistenerRef $ unlistenerB
                      pure unit)
                    pure do
                      join $ readRef unlistenerRef
                      unlistenerA)

instance monadBehavior :: Monad (Behavior e)

newBehavior :: forall e a. a -> ReactiveR e { behavior :: Behavior e a
                                            , push :: a -> ReactiveR e Unit }
newBehavior a = do es <- newEvent
                   pure { behavior : a `stepperR` es.event, push : es.push }

hold :: forall a e. a -> Event e a -> ReactiveR e (Behavior e a)
hold a e = pure $ a `stepperR` e

updates :: forall e a. Behavior e a -> Event e a
updates (Behavior b) = b.event

value :: forall e a. Behavior e a -> Event e a
value ba = Event \listener -> do
  es <- newEvent
  activateB ba -- Only activates behavior if eixits listener to prevent memory leak.
  a0 <- sample ba
  unlistener <- listenTrans (es.event <> updates ba) listener
  es.push a0
  pure do
    unlistener
    deactivateB ba
  where
    deactivateB (Behavior b) = do
      c <- readRef b.listenCounter
      modifyRef b.listenCounter (\x -> x - one)
      if c == zero
        then do
          join $ readRef b.deactivate
          writeRef b.deactivate $ pure unit
        else pure unit
    activateB (Behavior b) = do
      c <- liftR $ readRef b.listenCounter
      if c == zero
        then do
          unlisten <- listenTrans b.event $
                      (\a' -> do liftR $ writeRef b.last a'
                                 pure unit)
          liftR $ writeRef b.deactivate unlisten
        else pure unit
      liftR $ modifyRef b.listenCounter (one +)

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

switch :: forall e a. Behavior e (Behavior e a) -> ReactiveR e (Behavior e a)
switch bba = do
  ba0 <- sample bba
  a0 <- sample ba0
  a0 `hold` Event \l -> do
    ba0 <- sample bba
    -- XXX retainB bba?
    unlisten <- listenTrans (value ba0) l
    unlistenRef <- liftR $ newRef unlisten
    unlistenB <- listenTrans (updates bba) \ba -> do
      liftR $ join $ readRef unlistenRef
      unlisten <- listenTrans (value ba) l
      liftR $ writeRef unlistenRef unlisten
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
accum a0 ef = do
  aRef <- liftR $ newRef a0
  es <- newEventI \push ->
    listenTrans ef \f -> do
      a <- liftR $ readRef aRef
      let a' = f a
      liftR $ writeRef aRef a'
      push a'
  a0 `hold` es.event

retainB :: forall e a. Behavior e a -> ReactiveR e (Eff (ref :: Ref | e) Unit)
retainB b = listen (value b) $ \_ -> pure unit

stepperR :: forall a e. a -> Event e a -> Behavior e a
stepperR a e = Behavior { last: unsafePerformEff $ newRef a
                        , event: e
                        , listenCounter: unsafePerformEff $ newRef zero
                        , deactivate: unsafePerformEff $ newRef $ pure unit
                        }

-- switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
-- switcherR r er = join (r `hold` er)
