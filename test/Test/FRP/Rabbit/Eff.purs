module Test.FRP.Rabbit.Eff
  ( effSpec
  ) where

import Debug.Trace

import FRP.Rabbit

import Test.Spec (describe, it)
import Test.Spec.Assertions

import Test.FRP.Rabbit.Util

effSpec =
  describe "Eff" do
    it "executeEff" do
      es <- newEvent
      a1 <- newAggregator
      a2 <- newAggregator
      e <- executeEff es.event
      a1.read >>= shouldEqual []
      a2.read >>= shouldEqual []
      es.push do
        a2.record 1
        pure 1
      a2.read >>= shouldEqual []

      release <- retain e
      es.push do
        a2.record 2
        pure 2
      a2.read >>= shouldEqual [2]

      unlisten <- listen e a1.record
      es.push do
        a2.record 3
        pure 3

      a1.read >>= shouldEqual [3]
      a2.read >>= shouldEqual [2, 3]

      release
      es.push do
        a2.record 4
        pure 4
      a1.read >>= shouldEqual [3, 4]
      a2.read >>= shouldEqual [2, 3, 4]

      unlisten
      es.push do
        a2.record 5
        pure 5
      a1.read >>= shouldEqual [3, 4]
      a2.read >>= shouldEqual [2, 3, 4]
