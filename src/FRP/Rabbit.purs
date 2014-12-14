module FRP.Rabbit where

import Data.Maybe
import Data.Tuple
import Control.Monad.Eff.Ref
import qualified VirtualDOM as V
import VirtualDOM.VTree
import FRP.Rabbit.Signal (Signal(..), runSignal)

-- runRabbit :: Signal m VTree
runRabbit vtree initCallback = do
  ref <- newRef Nothing
  runSignal vtree $ \newVNode -> do
    state <- readRef ref
    case state of
      Nothing -> do
        let node = V.createElement newVNode
        initCallback node
        writeRef ref $ Just $ Tuple node newVNode
      Just (Tuple node prevVNode) -> do
        V.patch (V.diff prevVNode newVNode) node
        writeRef ref $ Just $ Tuple node newVNode