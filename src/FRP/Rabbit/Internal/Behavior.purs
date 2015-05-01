module FRP.Rabbit.Internal.Behavior
  ( sinkR
  , sinkRI
  , Behavior()
  , stepperR
  , switcherR
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Bind (join, ifM)
import Data.Monoid
import Data.Maybe
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Internal.Event
import Control.Monad.Cont.Trans

newtype Behavior e a = Behavior { value :: (RefVal a)
                                , event :: (Event e a) }

sinkR :: forall e a. Sink e a -> Behavior e a -> WithRef e (WithRef e Unit)
sinkR snk r = do
  res <- sinkRI (\a -> return $ snk a) r
  res.after
  return res.unsink

sinkRI :: forall e a. SinkI e a -> Behavior e a -> WithRef e { after :: (WithRef e Unit), unsink :: (WithRef e Unit)}
sinkRI snk (Behavior react) = do
  a <- readRef react.value
  unsnk <- sinkEI (\a -> do
                      writeRef react.value a
                      snk a) react.event
  after <- snk a
  return { after: after
         , unsink: unsnk }

instance functorBehavior :: Functor (Behavior e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeBehavior :: Applicative (Behavior e) where
  pure a = Behavior { value: unsafePerformEff $ newRef a
                    , event: (mempty :: Event e _) }

instance applyBehavior :: Apply (Behavior e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindBehavior :: Bind (Behavior e) where
  (>>=) ra k = Behavior
    { value: unsafePerformEff $ do
                  a <- sampleR ra
                  b <- sampleR $ k a
                  newRef b
    , event: Event $ ContT $ \snk -> do
                  unsnkRef <- newRef Nothing
                  res <- sinkRI (\a' -> do
                    unsink unsnkRef
                    new <- sinkRI (\b' -> snk b') $ k a'
                    writeRef unsnkRef $ Just new.unsink
                    return new.after) ra
                  return $ do
                    unsink unsnkRef
                    res.unsink }
    where unsink ref = readRef ref >>= maybe (return unit) id

sampleR :: forall e a. Behavior e a -> WithRef e a
sampleR (Behavior ra) = readRef ra.value

instance monadBehavior :: Monad (Behavior e)

stepperR :: forall a e. a -> Event e a -> Behavior e a
stepperR a e = Behavior
  { value: (unsafePerformEff $ do
               value <- newRef a
               sinkEI (\a' -> do -- unsink or leak?
                          writeRef value a'
                          return $ return unit) e
               return value)
  , event: e }

switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
switcherR r er = join (r `stepperR` er)
