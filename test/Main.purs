module Test.Main where

import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions

import Test.FRP.Rabbit.Event (eventSpec)
import Test.FRP.Rabbit.Behavior (behaviorSpec)
import Test.FRP.Rabbit.Eff (effSpec)
import Test.FRP.Rabbit.Internal.Event (internalEventSpec)
import Test.FRP.Rabbit.Internal.Behavior (internalBehaviorSpec)

main = runNode $
  describe "FRP.Rabbit" do
    eventSpec
    behaviorSpec
    effSpec
    internalEventSpec
    internalBehaviorSpec
