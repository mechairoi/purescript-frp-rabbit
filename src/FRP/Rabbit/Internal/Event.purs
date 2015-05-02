module FRP.Rabbit.Internal.Event
  ( Event(..)
  , newEvent
  , listen
  , never
  , merge
  , filterJust
  , listenI
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Monoid
import Data.Maybe
import Data.Traversable (sequence)
import Data.Foldable (sequence_)
import Data.Array (reverse)
import Control.Monad.Cont.Trans

import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Reactive

newtype Event e a = Event (ContT (Eff (ref :: Ref | e) Unit) (Eff (ref :: Ref | e)) a)

-- Listener is after callback
-- ListenerI returns after callback
-- runContT returns unlistener(unsubscriber/unregisterer)
-- listenerE returns unlistener
listen :: forall e a. Event e a -> Listener e a -> Reactive e (Unlistener e)
listen ea listener = listenI ea \a -> return $ listener a

-- listenI returns unlistener
listenI :: forall e a. Event e a -> ListenerI e a -> Reactive e (Unlistener e)
listenI (Event cont) listener = Reactive $ runContT cont listener

newEvent :: forall e a. Reactive e { event :: Event e a, push :: a -> Reactive e Unit }
newEvent = Reactive $ do
  listenerRefsRef <- newRef []
  let event = Event $ ContT $ \listener -> do
        listenerRef <- newRef listener
        modifyRef listenerRefsRef (listenerRef :)
        return $ do -- unlistener
          modifyRef listenerRefsRef $ removeOnce (listenerRef ==)
          writeRef listenerRef $ const $ return $ return unit
  let push = \a -> Reactive $ do
        listenerRefs <- readRef listenerRefsRef
        afters <- sequence $ readRef >>> (>>= ($ a)) <$> (reverse listenerRefs)
        sequence_ $ afters
  return { event: event, push: push }

instance monoidEvent :: Monoid (Event e a) where
  mempty = never

never :: forall e a. Event e a
never = Event $ ContT $ const $ return $ return unit

instance functorEvent :: Functor (Event e) where
  (<$>) f (Event ca) = Event $ ContT $ \g -> runContT ca (f >>> g)

instance semigroupEvent :: Semigroup (Event e a) where
  (<>) = merge

merge :: forall e a. Event e a -> Event e a -> Event e a
merge (Event ca) (Event cb) = Event $ ContT $ \f -> do
    unlistenerA <- runContT ca f
    unlistenerB <- runContT cb f
    return do
      unlistenerA
      unlistenerB

filterJust :: forall e a. Event e (Maybe a) -> Event e a
filterJust (Event cma) = Event $ ContT $ \f ->
  runContT cma \ma -> maybe (pure $ pure unit) f ma

