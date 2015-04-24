module FRP.Rabbit.Reactive
  ( sinkR
  , sinkRI
  , Reactive()
  , stepperR
  , switcherR
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Bind (join, ifM)
import Data.Monoid
import Data.Maybe
import FRP.Rabbit.Internal.Util
import FRP.Rabbit.Event
import Control.Monad.Cont.Trans

newtype Reactive e a = Reactive { value :: (RefVal a)
                                , event :: (Event e a) }

sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
sinkR snk r = do
  res <- sinkRI (\a -> return $ snk a) r
  res.after
  return res.unsink

sinkRI :: forall e a. SinkI e a -> Reactive e a -> WithRef e { after :: (WithRef e Unit), unsink :: (WithRef e Unit)}
sinkRI snk (Reactive react) = do
  a <- readRef react.value
  unsnk <- sinkEI (\a -> do
                      writeRef react.value a
                      snk a) react.event
  after <- snk a
  return { after: after
         , unsink: unsnk }

instance functorReactive :: Functor (Reactive e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeReactive :: Applicative (Reactive e) where
  pure a = Reactive { value: unsafePerformEff $ newRef a
                    , event: (mempty :: Event e _) }

instance applyReactive :: Apply (Reactive e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

foreign import consoleLog "function consoleLog(x) { console.log(x); return x }" :: forall a. a -> a

instance bindReactive :: Bind (Reactive e) where
  (>>=) ra k = Reactive
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

unsink ref = readRef ref >>= maybe (return unit) id

sampleR :: forall e a. Reactive e a -> WithRef e a
sampleR (Reactive ra) = readRef ra.value

instance monadReactive :: Monad (Reactive e)

stepperR :: forall a e. a -> Event e a -> Reactive e a
stepperR a e = Reactive
  { value: (unsafePerformEff $ do
               value <- newRef a
               sinkEI (\a' -> do -- unsink or leak?
                          writeRef value a'
                          return $ return unit) e
               return value)
  , event: e }

switcherR :: forall a e. Reactive e a -> Event e (Reactive e a) -> Reactive e a
switcherR r er = join (r `stepperR` er)
