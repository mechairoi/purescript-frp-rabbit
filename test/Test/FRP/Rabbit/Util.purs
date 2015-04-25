module Test.FRP.Rabbit.Util
  ( newAggregator
  ) where

import Control.Monad.Eff.Ref
import Data.Array (snoc)

newAggregator = do
  sumRef <- newRef []
  return {
    record: \x -> modifyRef sumRef (flip snoc x),
    read: readRef sumRef
  }
