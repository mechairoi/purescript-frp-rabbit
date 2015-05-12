module Test.FRP.Rabbit.Behavior
  ( behaviorSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

listenB b = listen (value b)

behaviorSpec =
  describe "behavior" do
    it "listenB -> push -> push " do
      bs <- sync $ newBehavior 1
      a <- newAggregator
      sync $ listenB bs.behavior a.record
      a.read >>= shouldEqual [1]
      sync $ bs.push 2
      a.read >>= shouldEqual [1, 2]
      sync $ bs.push 3
      a.read >>= shouldEqual [1, 2, 3]

    it "push -> listenB -> push -> push (invalid usage)" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ 1 `hold` es.event
      sync $ es.push 2
      sync $ listenB r a.record
      a.read >>= shouldEqual [1]
      sync $ es.push 3
      a.read >>= shouldEqual [1, 3]
      sync $ es.push 4
      a.read >>= shouldEqual [1, 3, 4]

    it "functorBehavior" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ 1 `hold` es.event
      sync $ listenB ((3 *) <$> r) a.record
      a.read >>= shouldEqual [3]
      sync $ es.push 2
      a.read >>= shouldEqual [3, 6]

    it "applicativeBehavior" do
      esa <- sync $ newEvent
      esf <- sync $ newEvent
      a <- newAggregator
      ra <- sync $ 1 `hold` esa.event
      rf <- sync $ (: [2]) `hold` esf.event
      release <- sync $ keep ra
      release <- sync $ keep rf
      sync $ listenB (rf <*> ra) a.record
      a.read >>= shouldEqual [[1, 2]]
      sync $ esa.push 2
      a.read >>= shouldEqual [[1, 2], [2, 2]]
      sync $ esa.push 3
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2]]
      sync $ esf.push (: [3])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3]]
      sync $ esf.push (: [4])
      a.read >>= shouldEqual [[1, 2], [2, 2], [3, 2], [3, 3], [3, 4]]

    it "bindBehavior" do
      esx <- sync $ newEvent
      esy <- sync $ newEvent
      a <- newAggregator
      rx <- sync $ 1 `hold` esx.event
      ry <- sync $ 1 `hold` esy.event
      release <- sync $ keep ry
      unlisten <- sync $ listenB (do
                         x <- rx
                         y <- ry
                         pure $ [x, y]) a.record
      a.read >>= shouldEqual [[1, 1]]

      sync $ esx.push 2
      a.read >>= shouldEqual [[1, 1], [2, 1]]

      sync $ esy.push 3
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3]]

      sync $ esy.push 4
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4]]

      sync $ esx.push 5
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      unlisten

      sync $ esx.push 6
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      sync $ esy.push 7
      a.read >>= shouldEqual [[1, 1], [2, 1], [2, 3], [2, 4], [5, 4]]

      release

    it "bind with self" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ 1 `hold` es.event
      release <- sync $ keep r
      sync $ listenB (do
        x <- r
        y <- r
        pure $ [x, y]) a.record
      a.read >>= shouldEqual [[1, 1]]

      sync $ es.push 2
      a.read >>= shouldEqual [[1, 1], [2, 2]]

    it "unlisten value" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ 1 `hold` es.event
      sync $ do
        unlisten <- listen (value r) a.record
        liftR $ unlisten
      a.read >>= shouldEqual []

    it "snapshot" do
      es <- sync $ newEvent
      bs <- sync $ newBehavior 1
      a <- newAggregator
      sync $ listen (snapshot (\a b -> [a, b]) es.event bs.behavior) a.record

      a.read >>= shouldEqual []

      sync $ bs.push 2
      a.read >>= shouldEqual []

      sync $ es.push 3
      a.read >>= shouldEqual [[3,2]]

      sync $ es.push 4
      a.read >>= shouldEqual [[3,2], [4,2]]

      sync $ bs.push 5
      a.read >>= shouldEqual [[3,2], [4,2]]

      sync $ es.push 6
      a.read >>= shouldEqual [[3,2], [4,2], [6,5]]

    it "collectE" do
      es <- sync $ newEvent
      a <- newAggregator
      r <- sync $ collectE (\a b -> a : b) [] es.event
      sync $ listen (value r) a.record
      -- a.read >>= shouldEqual [[]]
      sync $ es.push 2
      -- a.read >>= shouldEqual [[], [2]]
      sync $ es.push 3
      a.read >>= shouldEqual [[], [2], [3, 2]]
