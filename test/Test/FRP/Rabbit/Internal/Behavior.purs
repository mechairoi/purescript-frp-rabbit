module Test.FRP.Rabbit.Internal.Behavior
  ( internalBehaviorSpec
  ) where

import Debug.Trace

import FRP.Rabbit.Internal.Event
import FRP.Rabbit.Internal.Behavior
import FRP.Rabbit.Internal.Reactive

import Test.Spec (describe, it, pending)
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

internalBehaviorSpec =
  describe "behavior internal" do
    pending "unlisten value in Reactive"
    -- it "unlisten value in Reactive" do
    --   es <- sync newEvent
    --   a <- newAggregator
    --   r <- sync $ 1 `hold` es.event
    --   sync do
    --     unlisten <- listen (value r) a.record
    --     liftR unlisten
    --   a.read >>= shouldEqual []
