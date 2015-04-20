module Test.Main where

import Test.Spec
import Test.Spec.Node
import Test.Spec.Assertions

import Test.FRP.Rabbit.Event (eventSpec)
import Test.FRP.Rabbit.Reactive (reactiveSpec)
import Test.FRP.Rabbit.Sugar (sugarSpec)

main = runNode $
  describe "FRP.Rabbit" do
    eventSpec
    reactiveSpec
    sugarSpec
