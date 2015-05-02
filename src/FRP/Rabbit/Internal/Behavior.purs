module FRP.Rabbit.Internal.Behavior
  ( Behavior()
  -- , listenB
  -- , listenBI
  , sample
  , hold
  , updates
  , value
  -- , snapshot
  -- , switchE
  -- , switch
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Cont.Trans
import Control.Bind (join, ifM)
import Data.Monoid
import Data.Maybe
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

newtype Behavior e a = Behavior { value :: (RefVal a)
                                , event :: (Event e a) }

value :: forall e a. Behavior e a -> Event e a
value (Behavior ba) = Event $ ContT $ (\listener -> do
                                          v <- readRef ba.value
                                          after <- listener v
                                          after
                                          sync $ listenI (updates (Behavior ba)) listener
                                          pure $ pure unit)

updates :: forall e a. Behavior e a -> Event e a
updates (Behavior b) = b.event -- XXX not all events

listenB :: forall e a. Behavior e a -> Listener e a -> Reactive e (Unlistener e)
listenB ba listener = Reactive $ do
  res <- sync $ listenBI ba (\a -> return $ listener a)
  res.after
  pure $ res.unlisten

listenBI :: forall e a. Behavior e a -> ListenerI e a -> Reactive e { after :: (WithRef e Unit), unlisten :: Unlistener e}
listenBI (Behavior ba) listener = Reactive $ do
  a <- readRef ba.value
  unlistener <- sync $ listenI ba.event (\a -> do
                                            writeRef ba.value a
                                            listener a)
  after <- listener a
  pure { after: after
       , unlisten: unlistener }

instance functorBehavior :: Functor (Behavior e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeBehavior :: Applicative (Behavior e) where
  pure a = Behavior { value: unsafePerformEff $ newRef a
                    , event: (mempty :: Event e _) }

instance applyBehavior :: Apply (Behavior e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindBehavior :: Bind (Behavior e) where
  (>>=) ba k = Behavior
    { value: unsafePerformEff $ do
                  a <- sync $ sample ba
                  b <- sync $ sample $ k a
                  newRef b
    , event: Event $ ContT $ \listener -> do
                  unlistenerRef <- newRef Nothing
                  res <- sync $ listenBI ba (\a' -> do
                    maybeRef unlistenerRef
                    new <- sync $ listenBI (k a') (\b' -> listener b')
                    writeRef unlistenerRef $ Just new.unlisten
                    pure new.after)
                  pure $ do
                    maybeRef unlistenerRef
                    res.unlisten }
    where maybeRef ref = readRef ref >>= maybe (pure unit) id

sample :: forall e a. Behavior e a -> Reactive e a
sample (Behavior ba) = Reactive $ readRef ba.value

instance monadBehavior :: Monad (Behavior e)

hold :: forall a e. a -> Event e a -> Reactive e (Behavior e a)
hold a e = Reactive $ do
  value <- newRef a
  sync $ listenI e $ (\a' -> do -- unlistener or leak?
                         writeRef value a'
                         pure $ pure unit)
  pure $ Behavior { value: value, event: e }

-- switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
-- switcherR r er = join (r `hold` er)
