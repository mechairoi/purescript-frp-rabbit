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
                         pure [x, y]) a.record
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

    it "bindBehavior2" do
      es1 <- newEvent
      es2 <- newEvent
      a <- newAggregator
      b1 <- 1 `hold` es1.event
      b2 <- 1 `hold` es2.event
      release <- retainB b1
      release <- retainB b2
      let b3 = (2 *) <$> b2
      listenB (do
        x <- b1
        y <- b3
        pure [x, y]) a.record
      a.read >>= shouldEqual [[1, 2]]

      es2.push 2
      a.read >>= shouldEqual [[1, 2], [1, 4]]

      es1.push 3
      a.read >>= shouldEqual [[1, 2], [1, 4], [3, 4]]

    it "bind with self" do
      es <- newEvent
      a <- newAggregator
      r <- 1 `hold` es.event
      release <- retainB r
      listenB (do
        x <- r
        y <- r
        pure [x, y]) a.record
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

    it "switchE" do
      a <- newAggregator
      es0 <- newEvent
      es1 <- newEvent
      bs <- newBehavior es0.event
      listen (switchE bs.behavior) a.record

      a.read >>= shouldEqual []

      es0.push 0
      es1.push 1
      a.read >>= shouldEqual [0]

      bs.push es1.event
      a.read >>= shouldEqual [0]

      es0.push 2
      es1.push 3
      a.read >>= shouldEqual [0, 3]

      bs.push es0.event
      es0.push 4
      es1.push 5
      a.read >>= shouldEqual [0, 3, 4]

    it "switch" do
      a <- newAggregator
      bs0 <- newBehavior 0
      bs1 <- newBehavior 1
      release0 <- retainB bs0.behavior
      release1 <- retainB bs1.behavior
      bbs <- newBehavior bs0.behavior
      b <- switch bbs.behavior
      listen (value b) a.record

      a.read >>= shouldEqual [0]

      bs0.push 2
      bs1.push 3
      a.read >>= shouldEqual [0, 2]

      bbs.push bs1.behavior
      a.read >>= shouldEqual [0, 2, 3]

      bs0.push 4
      bs1.push 5
      a.read >>= shouldEqual [0, 2, 3, 5]

      bbs.push bs0.behavior
      a.read >>= shouldEqual [0, 2, 3, 5, 4]

      bs0.push 6
      bs1.push 7
      a.read >>= shouldEqual [0, 2, 3, 5, 4, 6]

      release0
      release1

    it "gate" do
      a <- newAggregator
      bs <- newBehavior false
      es <- newEvent
      listen (gate es.event bs.behavior) a.record
      a.read >>= shouldEqual []

      es.push 0
      a.read >>= shouldEqual []

      bs.push true
      a.read >>= shouldEqual []

      es.push 1
      a.read >>= shouldEqual [1]

      bs.push false
      a.read >>= shouldEqual [1]

      es.push 2
      a.read >>= shouldEqual [1]

    it "collectE" do
      es <- newEvent
      a <- newAggregator
      r <- collectE (\a b -> a : b) [] es.event
      listen (value r) a.record
      a.read >>= shouldEqual [[]]
      es.push 2
      a.read >>= shouldEqual [[], [2]]
      es.push 3
      a.read >>= shouldEqual [[], [2], [3, 2]]

    it "collect" do
      bs <- newBehavior 0
      a <- newAggregator
      r <- collect (\a b -> a : b) [] bs.behavior
      listen (value r) a.record
      a.read >>= shouldEqual [[0]]
      bs.push 2
      a.read >>= shouldEqual [[0], [2, 0]]
      bs.push 3
      a.read >>= shouldEqual [[0], [2, 0], [3, 2, 0]]

    it "accum" do
      es <- newEvent
      a <- newAggregator
      r <- accum 0 es.event
      listen (value r) a.record
      a.read >>= shouldEqual [0]
      es.push (+ 1)
      a.read >>= shouldEqual [0, (0 + 1)]
      es.push (+ 2)
      a.read >>= shouldEqual [0, (0 + 1), (0 + 1 + 2)]
