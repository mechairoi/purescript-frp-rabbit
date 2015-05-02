module Test.FRP.Rabbit.Sugar
  ( sugarSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Sugar
import FRP.Rabbit.Internal.Reactive

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

sugarSpec =
  describe "sugar" do
    it "collectE" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ collectE (\a b -> a : b) [] es.event
      sync $ listenB r a.record
      a.read >>= shouldEqual [[]]
      sync $ es.push 2
      a.read >>= shouldEqual [[], [2]]
      sync $ es.push 3
      a.read >>= shouldEqual [[], [2], [3, 2]]
