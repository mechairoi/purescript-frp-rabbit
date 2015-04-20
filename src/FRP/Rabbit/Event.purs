module FRP.Rabbit.Event
  ( Event()
  , newEventWithSource
  , sinkE
  , sinkEI
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Monoid
import Data.Profunctor (lmap)
import Data.Traversable (sequence)
import Data.Foldable (sequence_)

import FRP.Rabbit.Internal.Util

data Mapped m a b = Mapped (a -> b) (m a)
foreign import data MappedForeign :: (* -> *) -> * -> *

mapped :: forall m a b. MappedForeign m b -> Mapped m a b
mapped = unsafeCoerce
mappedForeign :: forall m a b. Mapped m a b -> MappedForeign m b
mappedForeign = unsafeCoerce

data Event e a = PureEvent (RefVal [ RefVal (SinkI e a) ])
               | AppendedEvent (Event e a) (Event e a)
               | MappedEvent (MappedForeign (Event e) a)

newEventWithSource :: forall e a. WithRef e { event :: Event e a, source :: Sink e a }
newEventWithSource = do
  cbRefsRef <- newRef []
  let event = PureEvent cbRefsRef
  let source = \a -> do
        cbRefs <- readRef cbRefsRef
        afters <- sequence $ readRef >>> (>>= ($ a)) <$> cbRefs
        sequence_ afters
  return { event: event, source: source }

sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
sinkE snk = sinkEI \a -> return $ snk a

sinkEI :: forall e a. SinkI e a -> Event e a -> WithRef e (WithRef e Unit)
sinkEI snk (PureEvent snksRef) = do
  snkRef <- newRef snk
  modifyRef snksRef (snkRef :)
  return $ do
    modifyRef snksRef $ removeOnce (snkRef ==)
    writeRef snkRef $ const $ return $ return unit
sinkEI snk (AppendedEvent ma mb) = do
  unregA <- sinkEI snk ma
  unregB <- sinkEI snk mb
  return $ do
    unregA
    unregB
sinkEI snk (MappedEvent e) = sink snk $ mapped e
  where
    sink :: forall e a b. SinkI e b -> Mapped (Event e) a b -> WithRef e (WithRef e Unit)
    sink snk (Mapped f ma) = sinkEI (lmap f snk) ma

instance monoidEvent :: Monoid (Event e a) where
  mempty = PureEvent $ unsafePerformEff $ newRef []

instance functorEvent :: Functor (Event e) where
  (<$>) f ma = MappedEvent $ mappedForeign $ Mapped f ma

instance semigroupEvent :: Semigroup (Event e a) where
  (<>) ma mb = AppendedEvent ma mb
