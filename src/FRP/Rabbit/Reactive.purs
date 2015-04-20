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

data Reactive e a = Stepper (RefVal a) (Event e a)
               | BindedReactive (BindedForeign (Reactive e) a)

data Binded m a b = Binded (m a) (a -> (m b))
foreign import data BindedForeign :: (* -> *) -> * -> *

binded :: forall m a b. BindedForeign m b -> Binded m a b
binded = unsafeCoerce
bindedForeign :: forall m a b. Binded m a b -> BindedForeign m b
bindedForeign = unsafeCoerce

sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
sinkR snk r = do
  res <- sinkRI (\a -> return $ snk a) r
  res.after
  return res.unreg

sinkRI :: forall e a. SinkI e a -> Reactive e a -> WithRef e { after :: (WithRef e Unit), unreg :: (WithRef e Unit)}
sinkRI snk (Stepper ar e) = do
  a <- readRef ar
  after <- snk a
  unreg <- sinkEI (\a -> do writeRef ar a
                            snk a) e
  return {
    after: after,
    unreg: unreg
  }

sinkRI snk (BindedReactive r) = do sink snk $ binded r
  where
    sink :: forall e a b. SinkI e b -> Binded (Reactive e) a b -> WithRef e { after :: (WithRef e Unit), unreg :: (WithRef e Unit) }
    sink snk (Binded ma k) = do
      unregRef <- newRef Nothing
      res <- sinkRI (\a -> do
        unregister unregRef
        new <- sinkRI (\a -> snk a) $ k a
        writeRef unregRef $ Just new.unreg
        return $ new.after) ma
      return $ {
        after: res.after,
        unreg: do
          unregister unregRef
          res.unreg
      }
      where unregister ref = readRef ref >>= maybe (return unit) id

foreign import consoleLog "function consoleLog(x) { console.log(JSON.stringify(x)); return x }" :: forall a. a -> a

instance functorReactive :: Functor (Reactive e) where
  (<$>) f ma = ma >>= (pure <<< f)

instance applicativeReactive :: Applicative (Reactive e) where
  pure a = Stepper (unsafePerformEff $ newRef a) mempty

instance applyReactive :: Apply (Reactive e) where
  (<*>) uf ua = uf >>= (\f -> ua >>= (pure <<< f))

instance bindReactive :: Bind (Reactive e) where
  (>>=) ma k = BindedReactive $ bindedForeign $ Binded ma k

instance monadReactive :: Monad (Reactive e)

stepperR :: forall a eff. a -> Event eff a -> Reactive eff a
stepperR a e = Stepper (unsafePerformEff $ newRef a) e

switcherR :: forall a eff. Reactive eff a -> Event eff (Reactive eff a) -> Reactive eff a
switcherR r er = join (r `stepperR` er)
