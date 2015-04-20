module Test.FRP.Rabbit.Sugar
  ( sugarSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Reactive
import FRP.Rabbit.Event
import FRP.Rabbit.Sugar (stateful)

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

sugarSpec =
  describe "sugar" do
    it "stateful" do
      es <- newEventWithSource
      a <- newAggregator
      r <- stateful (\a b -> a + b) 10 es.event
      sinkR a.add r
      a.read >>= shouldEqual 10
      es.source 2
      a.read >>= shouldEqual (10 + (10 + 2))
      es.source 3
      a.read >>= shouldEqual (10 + (10 + 2) + (10 + 2 + 3))
