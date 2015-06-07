module FRP.Rabbit.Internal.Eff (
  executeEff
) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

executeEff :: forall e a. Event e (Eff (ref :: Ref | e) a) -> ReactiveR e (Event e a)
executeEff ea = do
  es <- newEventI \push ->
    listenTrans ea \effa -> do
      a <- liftR effa
      push a
  pure es.event
