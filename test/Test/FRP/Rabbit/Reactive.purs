module Test.FRP.Rabbit.Reactive
  ( reactiveSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Reactive
import FRP.Rabbit.Event

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

reactiveSpec =
  describe "reactive" do
    it "sinkR -> source" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a.add r
      a.read >>= shouldEqual 1
      es.source 2
      a.read >>= shouldEqual 3

    it "functorReactive" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a.add $ (3 *) <$> r
      a.read >>= shouldEqual (3 * 1)
      es.source 2
      a.read >>= shouldEqual (3 * 1 + 3 * 2)

    it "applicativeReactive" do
      esa <- newEventWithSource
      esf <- newEventWithSource
      a <- newAggregator
      let ra = 1 `stepperR` esa.event
      let rf = (2 *) `stepperR` esf.event
      sinkR a.add $ rf <*> ra
      a.read >>= shouldEqual 2
      esa.source 2
      a.read >>= shouldEqual (2 + 2 * 2)
      esa.source 3
      a.read >>= shouldEqual (2 + 2 * 2 + 2 * 3)
      esf.source (3 *)
      a.read >>= shouldEqual (2 + 2 * 2 + 2 * 3 + 3 * 3)
      esf.source (4 *)
      a.read >>= shouldEqual (2 + 2 * 2 + 2 * 3 + 3 * 3 + 4 * 3)

    it "bindReactive" do
      esx <- newEventWithSource
      esy <- newEventWithSource
      a <- newAggregator
      let rx = 1 `stepperR` esx.event
      let ry = 1 `stepperR` esy.event
      sinkR a.add $ do
        x <- rx
        y <- ry
        return $ x * y
      a.read >>= shouldEqual 1

      esx.source 2
      a.read >>= shouldEqual (1 + 2 * 1)

      esy.source 3
      a.read >>= shouldEqual (1 + 2 * 1 + 2 * 3)

      esy.source 4
      a.read >>= shouldEqual (1 + 2 * 1 + 2 * 3 + 2 * 4)

      esx.source 5
      a.read >>= shouldEqual (1 + 2 * 1 + 2 * 3 + 2 * 4 + 5 * 4)

    it "bind with self" do
      es <- newEventWithSource
      a1 <- newAggregator
      a2 <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a1.add $ do
        x <- r
        y <- r
        return $ (x + 1) * y
      sinkR a2.add $ do
        x <- r
        y <- r
        return $ (y + 1) * x
      a1.read >>= shouldEqual 2
      a2.read >>= shouldEqual 2

      es.source 2
      a1.read >>= shouldEqual (2 + 3 * 2)
      a2.read >>= shouldEqual (2 + 2 * 3)
