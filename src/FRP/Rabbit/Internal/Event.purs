module FRP.Rabbit.Internal.Event
  ( Event(..)
  , newEvent
  , newEventI
  , listen
  , listenTrans
  , never
  , merge
  , filterJust
  , once
  , filterE
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Bind (join)
import Data.Monoid
import Data.Maybe
import Data.Array (reverse)

import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Reactive

newtype Event e a = Event ((a -> (ReactiveR e Unit)) -> ReactiveR e (Eff (ref :: Ref | e) Unit))


-- | Keep the event as active.
-- | The `retain` function returns the function to release.
-- |
-- | JavaScript has no weak reference. So we have to manually manage activity
-- | of `Event`s. To prevent memory leak, `Event`s are activated only if
-- | one or more listeners are exist (like reference counting). This function
-- | simply registers a dummy no-op lisetener.
retain :: forall e a. Event e a -> ReactiveR e (Eff (ref :: Ref | e) Unit)
retain ea = listen ea $ \_ -> pure unit

listen :: forall e a. Event e a
          -> Listener e a
          -> ReactiveR e (Eff (ref :: Ref | e) Unit)
listen ea listener = listenTrans ea $ \a -> Reactive $ pure { r: unit, after: listener a }

listenTrans :: forall e a. Event e a
               -> (a -> ReactiveR e Unit)
               -> ReactiveR e (Eff (ref :: Ref | e) Unit)
listenTrans (Event f) a = f a

newEvent :: forall e a. ReactiveR e { event :: Event e a
                                    , push :: a -> ReactiveR e Unit }
newEvent = newEventI \push -> pure $ pure unit

newEventI :: forall e a. ((a -> ReactiveR e Unit) -> ReactiveR e (Eff (ref :: Ref | e) Unit))
             -> ReactiveR e { event :: Event e a
                            , push :: a -> ReactiveR e Unit }
newEventI activate = liftR $ do
  listenerRefsRef <- newRef []
  deactivateRef <- newRef $ return unit
  let push = \a -> do
        listenerRefs <- liftR $ readRef listenerRefsRef
        sequenceR $ (\ref -> do
          l <- liftR $ readRef ref
          maybe (pure unit) ($ a) l) <$> (reverse listenerRefs)
        pure unit
  let event = Event $ \listener -> do
        listenerRef <- liftR $ newRef Nothing
        liftR $ do
          writeRef listenerRef $ Just $ \a ->
            mapAfter (\after -> do
                         l <- readRef listenerRef
                         maybe (pure unit) (const after) l) $ listener a
          modifyRef listenerRefsRef (listenerRef :)
        refs <- liftR $ readRef listenerRefsRef
        case refs of
          (_:[]) -> do
            deactivate <- activate push
            liftR $ writeRef deactivateRef deactivate
          _      -> return unit
        pure $ do -- unlistener
          modifyRef listenerRefsRef $ removeOnce (listenerRef ==)
          writeRef listenerRef Nothing
          case refs of
            [] -> join $ readRef deactivateRef
            _  -> return unit
  pure { event: event, push: push }

instance monoidEvent :: Monoid (Event e a) where
  mempty = never

never :: forall e a. Event e a
never = Event $ const $ pure $ pure unit

instance functorEvent :: Functor (Event e) where
  (<$>) f ea = Event $ \l -> listenTrans ea (f >>> l)

instance semigroupEvent :: Semigroup (Event e a) where
  (<>) = merge

merge :: forall e a. Event e a -> Event e a -> Event e a
merge ea eb = Event $ \l -> do
    unlistenerA <- listenTrans ea l
    unlistenerB <- listenTrans eb l
    pure do
      unlistenerA
      unlistenerB

filterJust :: forall e a. Event e (Maybe a) -> Event e a
filterJust ea = Event $ \l ->
  listenTrans ea \a -> maybe (pure unit) l a

once :: forall e a. Event e a -> Event e a
once ea = Event $ \l -> do
  first <- liftR $ newRef true
  unlistenerRef <- liftR $ newRef $ pure unit
  unlistener <- listenTrans ea \a -> do
    isFirst <- liftR $ readRef first
    if isFirst
      then do
        liftR $ writeRef first false
        l a
      else liftR $ join $ readRef unlistenerRef
  liftR $ writeRef unlistenerRef $ unlistener
  pure unlistener

filterE :: forall e a. (a -> Boolean) -> Event e a -> Event e a
filterE pred ea = filterJust $ (\a -> if (pred a) then Just a else Nothing) <$> ea
