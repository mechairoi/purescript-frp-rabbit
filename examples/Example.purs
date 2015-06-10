module Example.FRP.Rabbit.Signal where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import DOM
import VirtualDOM.VTree
import FRP.Rabbit.VirtualDOM (runBehaviorVTree)
import FRP.Rabbit

main = windowOnLoad $ do
  rootVNode >>= runBehaviorVTree >>= documentBodyAppendChild
  rootVNode >>= runBehaviorVTree >>= documentBodyAppendChild

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

rootVNode :: Eff _ (Behavior _ VTree)
rootVNode = do
  es <- newEvent
  state <- collectE (\e state -> increment state) initialState es.event
  return $ do
    s <- state
    return $ vnode "div" {}
      [ vnode "p" {} [ vtext $ show s.counter ]
      , vnode "button" { "onclick": handlerWrapper es.push } [ vtext "++" ]
      ]

foreign import data Handler :: *
foreign import handlerWrapper """
  function handlerWrapper(handler) {
    return function(event) {
      handler(event)();
    };
  }
  """ :: forall eff a. (a -> Eff eff Unit) -> Handler
