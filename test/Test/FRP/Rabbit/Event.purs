module Test.FRP.Rabbit.Event
  ( eventSpec
  ) where

import Debug.Trace
import Data.Maybe

import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive
import FRP.Rabbit.Class

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

    it "push -> listen" do
      es <- sync $ newEvent
      a <- newAggregator
      sync $ es.push 2
      sync $ listen es.event a.record
      a.read >>= shouldEqual []

    it "listen -> listen -> push" do
      es <- sync $ newEvent
      a1 <- newAggregator
      a2 <- newAggregator
      sync $ listen es.event a1.record
      sync $ listen es.event ((2 *) >>> a2.record)
      sync $ es.push 2
      a1.read >>= shouldEqual [2]
      a2.read >>= shouldEqual [4]

    it "listen -> push -> unlisten -> push" do
      a <- newAggregator
      es <- sync $ newEvent
      unlisten <- sync $ listen es.event a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2]
      unlisten
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

    it "unlisten in Reactive" do
      es <- sync $ newEvent
      a <- newAggregator
      sync $ do
        unlisten <- listen es.event a.record
        es.push 2
        liftR $ unlisten
      a.read >>= shouldEqual []

    it "once" do
      a <- newAggregator
      es <- sync $ newEvent
      sync $ listen (once es.event) a.record
      sync $ es.push 2
      a.read >>= shouldEqual [2]

      sync $ es.push 3
      a.read >>= shouldEqual [2]

    it "filterJust" do
      a <- newAggregator
      es <- sync $ newEvent
      sync $ listen (filterJust es.event) a.record
      sync $ es.push $ Just 2
      a.read >>= shouldEqual [2]

      sync $ es.push $ Nothing
      a.read >>= shouldEqual [2]

      sync $ es.push $ Just 3
      a.read >>= shouldEqual [2, 3]

    it "filterE" do
      a <- newAggregator
      es <- sync $ newEvent
      let even = (\x -> x % 2 == 0)
      sync $ listen (filterE even es.event) a.record
      sync $ es.push $ 2
      a.read >>= shouldEqual [2]

      sync $ es.push $ 3
      a.read >>= shouldEqual [2]

      sync $ es.push $ 4
      a.read >>= shouldEqual [2, 4]

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
