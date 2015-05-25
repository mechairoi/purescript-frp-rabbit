module Test.FRP.Rabbit.Behavior
  ( behaviorSpec
  ) where

import Debug.Trace

import FRP.Rabbit

import Test.Spec (describe, it)
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

listenB b = listen (value b)

behaviorSpec =
  describe "behavior" do
    it "listenB -> push -> push " do
      bs <- newBehavior 1
      a <- newAggregator
      listenB bs.behavior a.record
      a.read >>= shouldEqual [1]
      bs.push 2
      a.read >>= shouldEqual [1, 2]
      bs.push 3
      a.read >>= shouldEqual [1, 2, 3]

    it "push -> listenB -> push -> push (invalid usage)" do
      es <- newEvent
      a <- newAggregator
      r <- 1 `hold` es.event
      es.push 2
      listenB r a.record
      a.read >>= shouldEqual [1]
      es.push 3
      a.read >>= shouldEqual [1, 3]
      es.push 4
      a.read >>= shouldEqual [1, 3, 4]

    it "functorBehavior" do
      es <- newEvent
      a <- newAggregator
      r <- 1 `hold` es.event
      listenB ((3 *) <$> r) a.record
      a.read >>= shouldEqual [3]
      es.push 2
      a.read >>= shouldEqual [3, 6]

    it "applicativeBehavior" do
      esa <- newEvent
      esf <- newEvent
      a <- newAggregator
      ra <- 1 `hold` esa.event
      rf <- (: [2]) `hold` esf.event
      release <- retainB ra
      release <- retainB rf
      listenB (rf <*> ra) a.record
      a.read >>= shouldEqual [[1, 2]]
      esa.push 2
      a.read >>= shouldEqual [[1, 2], [2, 2]]
      esa.push 3
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2]]
      esf.push (: [3])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3]]
      esf.push (: [4])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3], [3, 4]]

    it "bindBehavior" do
      esx <- newEvent
      esy <- newEvent
      a <- newAggregator
      rx <- 1 `hold` esx.event
      ry <- 1 `hold` esy.event
      release <- retainB ry
      unlisten <- listenB (do
                         x <- rx
                         y <- ry
                         pure $ [x, y]) a.record
      a.read >>= shouldEqual [[1, 1]]

      esx.push 2
      a.read >>= shouldEqual [[1, 1], [2, 1]]

      esy.push 3
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3]]

      esy.push 4
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4]]

      esx.push 5
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      unlisten

      esx.push 6
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      esy.push 7
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      release

    it "bind with self" do
      es <- newEvent
      a <- newAggregator
      r <- 1 `hold` es.event
      release <- retainB r
      listenB (do
        x <- r
        y <- r
        pure $ [x, y]) a.record
      a.read >>= shouldEqual [[1, 1]]

      es.push 2
      a.read >>= shouldEqual [[1, 1], [2, 2]]

    it "snapshot" do
      es <- newEvent
      bs <- newBehavior 1
      a <- newAggregator
      listen (snapshot (\a b -> [a, b]) es.event bs.behavior) a.record

      a.read >>= shouldEqual []

      bs.push 2
      a.read >>= shouldEqual []

      es.push 3
      a.read >>= shouldEqual [[3,2]]

      es.push 4
      a.read >>= shouldEqual [[3,2], [4,2]]

      bs.push 5
      a.read >>= shouldEqual [[3,2], [4,2]]

      es.push 6
      a.read >>= shouldEqual [[3,2], [4,2], [6,5]]

    it "collectE" do
      es <- newEvent
      a <- newAggregator
      r <- collectE (\a b -> a : b) [] es.event
      listen (value r) a.record
      -- a.read >>= shouldEqual [[]]
      es.push 2
      -- a.read >>= shouldEqual [[], [2]]
      es.push 3
      a.read >>= shouldEqual [[], [2], [3, 2]]
