module Test.FRP.Rabbit.Behavior
  ( behaviorSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Event

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

behaviorSpec =
  describe "behavior" do
    it "sinkR -> source" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a.record r
      a.read >>= shouldEqual [1]
      es.source 2
      a.read >>= shouldEqual [1, 2]

    it "source -> sinkR -> source" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      es.source 2
      sinkR a.record r
      a.read >>= shouldEqual [2]
      es.source 3
      a.read >>= shouldEqual [2, 3]

    it "functorBehavior" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a.record $ (3 *) <$> r
      a.read >>= shouldEqual [3]
      es.source 2
      a.read >>= shouldEqual [3, 6]

    it "applicativeBehavior" do
      esa <- newEventWithSource
      esf <- newEventWithSource
      a <- newAggregator
      let ra = 1 `stepperR` esa.event
      let rf = (: [2]) `stepperR` esf.event
      sinkR a.record $ rf <*> ra
      a.read >>= shouldEqual [[1, 2]]
      esa.source 2
      a.read >>= shouldEqual [[1, 2], [2, 2]]
      esa.source 3
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2]]
      esf.source (: [3])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3]]
      esf.source (: [4])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3], [3, 4]]

    it "bindBehavior" do
      esx <- newEventWithSource
      esy <- newEventWithSource
      a <- newAggregator
      let rx = 1 `stepperR` esx.event
      let ry = 1 `stepperR` esy.event
      sinkR a.record $ do
        x <- rx
        y <- ry
        return $ [x, y]
      a.read >>= shouldEqual [[1, 1]]

      esx.source 2
      a.read >>= shouldEqual [[1, 1], [2, 1]]

      esy.source 3
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3]]

      esy.source 4
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4]]

      esx.source 5
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

    it "bind with self" do
      es <- newEventWithSource
      a <- newAggregator
      let r = 1 `stepperR` es.event
      sinkR a.record $ do
        x <- r
        y <- r
        return $ [x, y]
      a.read >>= shouldEqual [[1, 1]]

      es.source 2
      a.read >>= shouldEqual [[1, 1], [2, 2]]
