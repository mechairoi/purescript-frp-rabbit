module FRP.Rabbit.Internal.Behavior
  ( Behavior()
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
import Control.Bind (join)
import Data.Monoid
import Data.Maybe
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

newtype Behavior e a = Behavior { last :: (RefVal a)
                                , event :: (Event e a) }

value :: forall e a. Behavior e a -> Event e a
value ba = Event \listener -> do
  es <- newEvent
  a0 <- sample ba
  unlistener <- listenTrans (es.event <> updates ba) listener
  es.push a0
  pure unlistener

-- internal
values :: forall e a. Behavior e a -> Event e a
values ba = Event \listener -> do
  es <- newEvent
  a0 <- sample ba
  unlistener <- listenTrans (es.event <> event ba) listener
  es.push a0
  pure unlistener

updates :: forall e a. Behavior e a -> Event e a
updates (Behavior b) = coalesce (\s a -> a) b.event

instance functorBehavior :: Functor (Behavior e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeBehavior :: Applicative (Behavior e) where
  pure a = Behavior { last: unsafePerformEff $ newRef a
                    , event: (mempty :: Event e _) }

instance applyBehavior :: Apply (Behavior e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindBehavior :: Bind (Behavior e) where
  (>>=) ba k = unsafePerformEff do
    a0 <- sync $ sample ba
    b0 <- sync $ sample $ k a0
    r0 <- newRef b0
    pure $ Behavior
      { last: r0
      , event: Event $ \listener -> do
                    let l = \b -> do liftR $ writeRef r0 b
                                     listener b
                    unlistenerB <- listenTrans (event $ k a0) l
                    unlistenerRef <- liftR $ newRef unlistenerB
                    unlistenerA <- listenTrans (event ba) (\a -> do
                      liftR $ readRef unlistenerRef >>= id
                      unlistenerB <- listenTrans (values $ k a) l
                      liftR $ writeRef unlistenerRef $ unlistenerB
                      pure unit)
                    pure $ do
                      readRef unlistenerRef >>= id
                      unlistenerA }

event :: forall e a. Behavior e a -> Event e a
event (Behavior b) = b.event

sample :: forall e a. Behavior e a -> ReactiveR e a
sample (Behavior ba) = liftR $ readRef ba.last

instance monadBehavior :: Monad (Behavior e)

hold :: forall a e. a -> Event e a -> ReactiveR e (Behavior e a)
hold a e = do
  last <- liftR $ newRef a
  listenTrans e $ (\a' -> do liftR $ writeRef last a' -- XXX unlisten?
                             pure unit)
  pure $ Behavior { last: last, event: e }

-- switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
-- switcherR r er = join (r `hold` er)
