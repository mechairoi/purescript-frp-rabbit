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

type Handler a eff = a -> (WithRef eff) Unit
type EventSignal a eff = Signal (WithRef eff) a

createEventHandler :: forall a eff2 eff. (WithRef eff) { handler :: Handler a eff2, event :: EventSignal a eff2 }
createEventHandler = do
  ref <- newRef []
  return { event: Signal (\callback -> do
                             cbs <- readRef ref
                             writeRef ref (callback : cbs)
                             return unit)
         , handler: \a -> do
              cbs <- readRef ref
              sequence $ (\cb -> cb a) <$> cbs
              return unit
         }
