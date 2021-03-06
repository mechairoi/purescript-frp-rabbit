module Test.FRP.Rabbit.Event
  ( eventSpec
  ) where

import Debug.Trace
import Data.Maybe

import FRP.Rabbit
import FRP.Rabbit.Internal.Util (unsafePerformEff)

import Test.Spec (describe, it, pending)
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

eventSpec = do
  describe "event" do
    it "listen -> push" do
      a <- newAggregator
      es <- newEvent
      listen es.event a.record
      es.push 2
      a.read >>= shouldEqual [2]

    it "listen -> push -> push" do
      a <- newAggregator
      es <- newEvent
      listen es.event a.record
      es.push 2
      a.read >>= shouldEqual [2]

      es.push 3
      a.read >>= shouldEqual [2, 3]

    it "push -> listen" do
      es <- newEvent
      a <- newAggregator
      es.push 2
      listen es.event a.record
      a.read >>= shouldEqual []

    it "listen -> listen -> push" do
      es <- newEvent
      a1 <- newAggregator
      a2 <- newAggregator
      listen es.event a1.record
      listen es.event ((2 *) >>> a2.record)
      es.push 2
      a1.read >>= shouldEqual [2]
      a2.read >>= shouldEqual [4]

    it "listen -> push -> unlisten -> push" do
      a <- newAggregator
      es <- newEvent
      unlisten <- listen es.event a.record
      es.push 2
      a.read >>= shouldEqual [2]
      unlisten
      es.push 3
      a.read >>= shouldEqual [2]

    it "functorEvent" do
      es <- newEvent
      a <- newAggregator
      listen ((2 *) <$> es.event) a.record

      es.push 2
      a.read >>= shouldEqual [4]

    it "semigroupEvent" do
      esa <- newEvent
      esb <- newEvent
      a <- newAggregator
      listen (esa.event <> esb.event) a.record

      esa.push 2
      a.read >>= shouldEqual [2]

      esb.push 3
      a.read >>= shouldEqual [2, 3]

      esa.push 4
      a.read >>= shouldEqual [2, 3, 4]

    it "semigroupEvent e <> e" do
      es <- newEvent
      a <- newAggregator
      listen (es.event <> es.event) a.record
      es.push 2
      a.read >>= shouldEqual [2, 2]

    it "filterJust" do
      a <- newAggregator
      es <- newEvent
      listen (filterJust es.event) a.record
      es.push $ Just 2
      a.read >>= shouldEqual [2]

      es.push Nothing
      a.read >>= shouldEqual [2]

      es.push $ Just 3
      a.read >>= shouldEqual [2, 3]

    it "once" do
      a <- newAggregator
      es <- newEvent
      listen (once es.event) a.record
      es.push 2
      a.read >>= shouldEqual [2]

      es.push 3
      a.read >>= shouldEqual [2]

    it "filterE" do
      a <- newAggregator
      es <- newEvent
      let even = (\x -> x % 2 == 0)
      listen (filterE even es.event) a.record
      es.push 2
      a.read >>= shouldEqual [2]

      es.push 3
      a.read >>= shouldEqual [2]

      es.push 4
      a.read >>= shouldEqual [2, 4]

    it "cache, retain" do
      a <- newAggregator
      a2 <- newAggregator
      a3 <- newAggregator
      a4 <- newAggregator
      es <- newEvent
      listen es.event a.record
      ea <- cache ((\x -> unsafePerformEff do
                       a2.record x
                       pure $ x * 2) <$> es.event)
      es.push 0
      a.read >>= shouldEqual [0]
      a2.read >>= shouldEqual []
      a3.read >>= shouldEqual []
      a4.read >>= shouldEqual []

      unlisten1 <- listen ea a3.record
      unlisten2 <- listen ea a4.record

      es.push 1
      a.read >>= shouldEqual [0, 1]
      a2.read >>= shouldEqual [1]
      a3.read >>= shouldEqual [2]
      a4.read >>= shouldEqual [2]

      unlisten1
      es.push 2

      a.read >>= shouldEqual [0, 1, 2]
      a2.read >>= shouldEqual [1, 2]
      a3.read >>= shouldEqual [2]
      a4.read >>= shouldEqual [2, 4]

      unlisten2
      es.push 3

      a.read >>= shouldEqual [0, 1, 2, 3]
      a2.read >>= shouldEqual [1, 2]
      a3.read >>= shouldEqual [2]
      a4.read >>= shouldEqual [2, 4]

      retain ea
      es.push 4

      a.read >>= shouldEqual [0, 1, 2, 3, 4]
      a2.read >>= shouldEqual [1, 2, 4]
      a3.read >>= shouldEqual [2]
      a4.read >>= shouldEqual [2, 4]

    it "no cache" do
      a <- newAggregator
      a2 <- newAggregator
      a3 <- newAggregator
      es <- newEvent
      let ea = (\x -> unsafePerformEff do
                       a.record x
                       pure $ x * 2) <$> es.event
      unlisten1 <- listen ea a2.record
      unlisten2 <- listen ea a3.record

      es.push 0
      a.read >>= shouldEqual [0, 0]
      a2.read >>= shouldEqual [0]
      a3.read >>= shouldEqual [0]

      unlisten1

      es.push 1
      a.read >>= shouldEqual [0, 0, 1]
      a2.read >>= shouldEqual [0]
      a3.read >>= shouldEqual [0, 2]
