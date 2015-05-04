module FRP.Rabbit.Internal.Event
  ( Event(..)
  , newEvent
  , listen
  , listenTrans
  , never
  , merge
  , filterJust
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Monoid
import Data.Maybe
import Data.Array (reverse)
import Control.Monad.Cont.Trans

import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Reactive

newtype Event e a = Event ((a -> (ReactiveR e Unit)) -> ReactiveR e (Eff (ref :: Ref | e) Unit))

listen :: forall e a. Event e a
          -> Listener e a
          -> ReactiveR e (Unlistener e)
listen ea listener = listenTrans ea $ \a -> Reactive $ pure { r: unit, after: listener a }

listenTrans :: forall e a. Event e a
               -> (a -> ReactiveR e Unit)
               -> ReactiveR e (Eff (ref :: Ref | e) Unit)
listenTrans (Event f) a = f a

newEvent :: forall e a. ReactiveR e { event :: Event e a, push :: a -> ReactiveR e Unit }
newEvent = liftR $ do
  listenerRefsRef <- newRef []
  let event = Event $ \listener -> liftR $ do
        listenerRef <- newRef Nothing
        writeRef listenerRef $ Just $ \a ->
          mapAfter (\after -> do
                       l <- readRef listenerRef
                       maybe (pure unit) (const after) l) $ listener a
        modifyRef listenerRefsRef (listenerRef :)
        pure $ do -- unlistener
          modifyRef listenerRefsRef $ removeOnce (listenerRef ==)
          writeRef listenerRef Nothing
  let push = \a -> do
        listenerRefs <- liftR $ readRef listenerRefsRef
        sequenceR $ (\ref -> do
          l <- liftR $ readRef ref
          maybe (pure unit) ($ a) l) <$> (reverse listenerRefs)
        pure unit
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
