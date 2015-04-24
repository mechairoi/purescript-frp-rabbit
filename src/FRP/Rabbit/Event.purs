module FRP.Rabbit.Event
  ( Event(..)
  , newEventWithSource
  , sinkE
  , sinkEI
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Monoid
import Data.Traversable (sequence)
import Data.Foldable (sequence_)
import Data.Array (reverse)
import Control.Monad.Cont.Trans

import FRP.Rabbit.Internal.Util

newtype Event e a= Event (ContT (Eff (ref :: Ref | e) Unit) (Eff (ref :: Ref | e)) a)

-- Sink is after callback
-- SinkI returns after callback
-- runContT returns unsink(unsubscriber/unregisterer)
-- sinkE returns unsink
sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
sinkE snk = sinkEI \a -> return $ snk a

-- sinkEI returns unsink
sinkEI :: forall e a. SinkI e a -> Event e a -> WithRef e (WithRef e Unit)
sinkEI snk (Event cont) = runContT cont snk

newEventWithSource :: forall e a. WithRef e { event :: Event e a, source :: Sink e a }
newEventWithSource = do
  snkRefsRef <- newRef []
  let event = Event $ ContT $ \snk -> do
        snkRef <- newRef snk
        modifyRef snkRefsRef (snkRef :)
        return $ do -- unsink
          modifyRef snkRefsRef $ removeOnce (snkRef ==)
          writeRef snkRef $ const $ return $ return unit
  let source = \a -> do
        snkRefs <- readRef snkRefsRef
        afters <- sequence $ readRef >>> (>>= ($ a)) <$> (reverse snkRefs)
        sequence_ $ afters
  return { event: event, source: source }

instance monoidEvent :: Monoid (Event e a) where
  mempty = Event $ ContT $ \_ -> return $ return unit

instance functorEvent :: Functor (Event e) where
  (<$>) f (Event ca) = Event $ ContT $ \g -> do
    runContT ca (f >>> g)

instance semigroupEvent :: Semigroup (Event e a) where
  (<>) (Event ca) (Event cb) = Event $ ContT $ \f -> do
    unsnkA <- runContT ca f
    unsnkB <- runContT cb f
    return do
      unsnkA
      unsnkB
