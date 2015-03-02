module FRP.Rabbit.Handler
  ( createEventHandler
  , Handler(..)
  , EventSignal(..)
  ) where

import Data.Array
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import FRP.Rabbit.Signal
import Data.Traversable(sequence)
import Data.Maybe

type Handler a eff = a -> (WithRef eff) Unit
type EventSignal a eff = Signal (WithRef eff) a

createEventHandler :: forall a eff2 eff. (WithRef eff) { handler :: Handler a eff2, event :: EventSignal a eff2 }
createEventHandler = do
  ref <- newRef []
  lastVal <- newRef Nothing
  return { event: Signal (\callback -> do
                             cbs <- readRef ref
                             writeRef ref (callback : cbs)

                             val <- readRef lastVal
                             case val of
                               Nothing -> return unit
                               Just v  -> callback v)
         , handler: \a -> do
              writeRef lastVal $ Just a
              cbs <- readRef ref
              sequence $ (\cb -> cb a) <$> cbs
              return unit
         }
