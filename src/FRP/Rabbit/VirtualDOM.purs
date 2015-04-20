module FRP.Rabbit.VirtualDOM
  ( runReactiveVTree
  ) where

import Data.Maybe
import Control.Monad.Eff.Ref
import qualified VirtualDOM as V
import VirtualDOM.VTree
import DOM
import FRP.Rabbit.Reactive
import FRP.Rabbit.Internal.Util

runReactiveVTree :: forall e. Reactive (dom :: DOM | e) VTree
                           -> WithRef (dom :: DOM | e) DOM.Node
runReactiveVTree rvtree = do
  ref <- newRef Nothing
  sinkR (\vnode' -> do
    state <- readRef ref
    case state of
      Nothing -> do
        let node = V.createElement vnode'
        writeRef ref $ Just { real: node, virtual: vnode' }
      Just { real: node, virtual: vnode } -> do
        V.patch (V.diff vnode vnode') node
        writeRef ref $ Just { real: node, virtual: vnode' }
    ) rvtree
  Just s <- readRef ref -- XXX
  let node = s.real :: DOM.Node
  return node
