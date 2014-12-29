module FRP.Rabbit.Handler
  ( createHandler
  , Handler(..)
  ) where

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import FRP.Rabbit.Signal
import Data.Traversable(sequence)

type Handler eff a = a -> (WithRef eff) Unit

createHandler :: forall eff eff2 a. (WithRef eff) { handler :: Handler eff2 a, signal :: Signal (WithRef eff2) a }
createHandler = do
  ref <- newRef []
  return { signal: Signal (\callback -> do
                             cbs <- readRef ref
                             writeRef ref (callback : cbs)
                             return unit)
         , handler: \a -> do
              cbs <- readRef ref
              sequence $ (\cb -> cb a) <$> cbs
              return unit
         }
