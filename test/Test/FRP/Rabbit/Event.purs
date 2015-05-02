module Test.FRP.Rabbit.Event
  ( eventSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

eventSpec = do
  describe "event" do
    it "listen -> push" do
      a <- newAggregator
      es <- sync $ newEvent
      sync $ listen es.event a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2]

    it "listen -> push -> push" do
      a <- newAggregator
      es <- sync $ newEvent
      sync $ listen es.event a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2]

      sync $ es.push 3
      a.read >>= shouldEqual [2, 3]

    it "push -> sink" do
      es <- sync $ newEvent
      a <- newAggregator
      sync $ es.push 2
      sync $ listen es.event a.record
      a.read >>= shouldEqual []

    it "sink -> sink -> push" do
      es <- sync $ newEvent
      a1 <- newAggregator
      a2 <- newAggregator
      sync $ listen es.event a1.record
      sync $ listen es.event ((2 *) >>> a2.record)
      sync $ es.push 2
      a1.read >>= shouldEqual [2]
      a2.read >>= shouldEqual [4]

    it "sink -> push -> unsink -> push" do
      a <- newAggregator
      es <- sync $ newEvent
      unsink <- sync $ listen es.event a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2]
      unsink
      sync $ es.push 3
      a.read >>= shouldEqual [2]

    it "functorEvent" do
      es <- sync $ newEvent
      a <- newAggregator
      sync $ listen ((2 *) <$> es.event) a.record

      sync $ es.push 2
      a.read >>= shouldEqual [4]

    it "semigroupEvent" do
      esa <- sync $ newEvent
      esb <- sync $ newEvent
      a <- newAggregator
      sync $ listen (esa.event <> esb.event) a.record

      sync $ esa.push 2
      a.read >>= shouldEqual [2]

      sync $ esb.push 3
      a.read >>= shouldEqual [2, 3]

      sync $ esa.push 4
      a.read >>= shouldEqual [2, 3, 4]

    it "semigroupEvent e <> e" do
      es <- sync $ newEvent
      a <- newAggregator
      sync $ listen (es.event <> es.event) a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2, 2]

  describe "event loop" do
    pending "make infinite loop."
    -- push <- do
    --   es <- sync $ newEvent
    --   sumRef <- newRef 0
    --   sync listen es.event (\x -> modifyRef sumRef ((+) x))
    --   return es.push
    -- loop push
    -- where
    --   loop :: forall eff. Sink eff Number -> Eff (ref :: Ref | eff) Unit
    --   loop push = do
    --     sync $ push 1
    --     loop push -- XXX tailrec?
