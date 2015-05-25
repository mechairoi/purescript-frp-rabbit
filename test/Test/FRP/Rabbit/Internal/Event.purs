module Test.FRP.Rabbit.Internal.Event
  ( internalEventSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Reactive

import Test.Spec (describe, it, pending)
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

internalEventSpec = do
  describe "event internal" do
    it "unlisten in Reactive" do
      es <- sync $ newEvent
      a <- newAggregator
      sync do
        unlisten <- listen es.event a.record
        es.push 2
        liftR unlisten
      a.read >>= shouldEqual []

  describe "event loop" do
    pending "make infinite loop."
    -- push <- do
    --   es <- newEvent
    --   sumRef <- newRef 0
    --   sync listen es.event (\x -> modifyRef sumRef ((+) x))
    --   return es.push
    -- loop push
    -- where
    --   loop :: forall eff. Sink eff Number -> Eff (ref :: Ref | eff) Unit
    --   loop push = do
    --     push 1
    --     loop push -- XXX tailrec?
