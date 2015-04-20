module Test.FRP.Rabbit.Event
  ( eventSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Event

import Test.Spec
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

eventSpec = do
  describe "event" do
    it "sink -> source" do
      a <- newAggregator
      es <- newEventWithSource
      sinkE a.add es.event
      es.source 2
      a.read >>= shouldEqual 2

    it "sink -> source -> source" do
      a <- newAggregator
      es <- newEventWithSource
      sinkE a.add es.event
      es.source 2
      a.read >>= shouldEqual 2

      es.source 3
      a.read >>= shouldEqual 5

    it "source -> sink" do
      es <- newEventWithSource
      a <- newAggregator
      es.source 2
      sinkE a.add es.event
      a.read >>= shouldEqual 0

    it "sink -> sink -> source" do
      es <- newEventWithSource
      a1 <- newAggregator
      a2 <- newAggregator
      sinkE a1.add es.event
      sinkE ((2 *) >>> a2.add) es.event
      es.source 2
      a1.read >>= shouldEqual 2
      a2.read >>= shouldEqual 4

    it "functorEvent" do
      es <- newEventWithSource
      a <- newAggregator
      sinkE a.add $ (2 *) <$> es.event

      es.source 2
      a.read >>= shouldEqual 4

    it "semigroupEvent" do
      esa <- newEventWithSource
      esb <- newEventWithSource
      a <- newAggregator
      sinkE a.add $ esa.event <> esb.event

      esa.source 2
      a.read >>= shouldEqual 2

      esb.source 3
      a.read >>= shouldEqual (2 + 3)

      esa.source 4
      a.read >>= shouldEqual (2 + 3 + 4)

    it "semigroupEvent e <> e" do
      es <- newEventWithSource
      a <- newAggregator
      sinkE a.add $ es.event <> es.event
      es.source 2
      a.read >>= shouldEqual 4

  describe "event loop" do
    pending "make infinite loop."
    -- source <- do
    --   es <- newEventWithSource
    --   sumRef <- newRef 0
    --   sinkE (\x -> modifyRef sumRef ((+) x)) es.event
    --   return es.source
    -- loop source
    -- where
    --   loop :: forall eff. Sink eff Number -> Eff (ref :: Ref | eff) Unit
    --   loop source = do
    --     source 1
    --     loop source -- XXX tailrec?
