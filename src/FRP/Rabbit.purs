module FRP.Rabbit
  ( Sink(), WithRef()
  , Event(), sinkE, newEventWithSource
  , Reactive(), sinkR, stepperR, switcherR
  , stateful
  ) where

import qualified FRP.Rabbit.Reactive as Reactive
import qualified FRP.Rabbit.Event as Event
import qualified FRP.Rabbit.Sugar as Sugar
import qualified FRP.Rabbit.Internal.Util as Util

import Control.Monad.Eff
import Control.Monad.Eff.Ref
type WithRef eff a = Eff (ref :: Ref | eff) a
type Sink eff a = a -> WithRef eff Unit

type Event = Event.Event
sinkE = Event.sinkE
newEventWithSource = Event.newEventWithSource

type Reactive = Reactive.Reactive
sinkR = Reactive.sinkR
stepperR = Reactive.stepperR
switcherR = Reactive.switcherR

stateful = Sugar.stateful
