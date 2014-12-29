module Test.FRP.Rabbit.Signal where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Trans
import DOM
import VirtualDOM.VTree.Typed
import FRP.Rabbit (runRabbit)
import FRP.Rabbit.Handler (createHandler)
import FRP.Rabbit.Signal (Signal(..), stateful)

main = windowOnLoad $ do
  runRabbit rootVNode documentBodyAppendChild
  runRabbit rootVNode documentBodyAppendChild

foreign import windowOnLoad """
  function windowOnLoad(callback) {
    return function() {
      window.onload = function() {
        callback();
      };
    };
  }
""" :: forall e e2. Eff e Unit -> Eff (dom :: DOM | e2) Unit

foreign import documentBodyAppendChild """
  function documentBodyAppendChild(node) {
    return function() {
      document.body.appendChild(node);
    };
  }
""" :: forall e. Node -> Eff (dom :: DOM | e) Unit

type State = { counter :: Number }

initialState :: State
initialState = { counter: 0 }

increment :: State -> State
increment state = state { counter = state.counter + 1 }

rootVNode :: (Signal (Eff (dom :: DOM, ref :: Ref)) VTree)
rootVNode = do
  eh <- lift $ createHandler
  let state = stateful (\e state -> increment state) initialState eh.signal
  s <- (return initialState) <> state
  return $ vnode "div" []
    [ vnode "p" [] [ vtext $ show s.counter ] Nothing Nothing
    , vnode "button"
        [ handler "onclick" eh.handler ]
        [ vtext "++" ]
        Nothing Nothing
    ] Nothing Nothing
