module FRP.Rabbit.Internal.Behavior
  ( Behavior()
  , newBehavior
  , sample
  , hold
  , updates
  , value
  , snapshot
  -- , switchE
  -- , switch
  , keep
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

newBehavior :: forall e a. a -> ReactiveR e { behavior :: (Behavior e a)
                                            , push :: a -> (ReactiveR e Unit) }
newBehavior a = do es <- newEvent
                   pure { behavior : a `stepperR` es.event, push : es.push }

-- | `keep` the behavior as active.
-- | This function returns the function to release.
-- |
-- | JavaScript has no weak reference. So we have to manually manage activity
-- | of `Behavior`s. To prevent memory leak, `Behavior`s are activated only if
-- | one or more listeners are exist (like reference counting). This function
-- | simply registers a dummy no-op lisetener.
keep :: forall e a. Behavior e a -> ReactiveR e (Eff (ref :: Ref | e) Unit)
keep b = listen (value b) $ \_ -> pure unit

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

updates :: forall e a. Behavior e a -> Event e a
updates (Behavior b) = b.event

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

sample :: forall e a. Behavior e a -> ReactiveR e a
sample (Behavior ba) = liftR $ readRef ba.last

instance monadBehavior :: Monad (Behavior e)

hold :: forall a e. a -> Event e a -> ReactiveR e (Behavior e a)
hold a e = pure $ a `stepperR` e

stepperR :: forall a e. a -> Event e a -> Behavior e a
stepperR a e = Behavior { last: unsafePerformEff $ newRef a
                        , event: e
                        , listenCounter: unsafePerformEff $ newRef zero
                        , deactivate: unsafePerformEff $ newRef $ pure unit
                        }

snapshot :: forall e a b c. (a -> b -> c)
            -> Event e a
            -> Behavior e b
            -> Event e c
snapshot f ea bb = Event \l -> do
  release <- keep bb
  unlisten <- listenTrans ea (\a -> do
    b <- sample bb
    l $ f a b
  )
  return do release
            unlisten

-- switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
-- switcherR r er = join (r `hold` er)

switchE :: forall e a. Behavior e (Event e a) -> Event e a
switchE bea = Event \l -> do
  unlistenRef <- liftR $ newRef Nothing
  unlistenB <- listenTrans (value bea) \ea -> do
    liftR $ readRef unlistenRef >>= maybe (pure unit) id
    unlisten <- listenTrans ea l
    liftR $ writeRef unlistenRef $ Just unlisten
  pure do
    readRef unlistenRef >>= maybe (pure unit) id
    unlistenB
