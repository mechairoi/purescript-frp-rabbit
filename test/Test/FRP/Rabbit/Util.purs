module Test.FRP.Rabbit.Util
  ( newAggregator
  ) where

import Control.Monad.Eff.Ref

newAggregator = do
  sumRef <- newRef 0
  return {
    add: \x -> modifyRef sumRef ((+) x),
    read: readRef sumRef
  }
