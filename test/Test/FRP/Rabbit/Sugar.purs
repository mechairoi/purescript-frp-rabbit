module Test.FRP.Rabbit.Sugar
  ( sugarSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Reactive
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Sugar (stateful)

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

sugarSpec =
  describe "sugar" do
    it "stateful" do
      es <- newEventWithSource
      a <- newAggregator
      r <- stateful (\a b -> a : b) [] es.event
      sinkR a.record r
      a.read >>= shouldEqual [[]]
      es.source 2
      a.read >>= shouldEqual [[], [2]]
      es.source 3
      a.read >>= shouldEqual [[], [2], [3, 2]]
