module FRP.Rabbit.Internal.Util where

import Control.Monad.Eff
import Control.Monad.Eff.Ref

type WithRef eff a = Eff (ref :: Ref | eff) a

type ListenerI eff a = a -> WithRef eff (WithRef eff Unit)
type Listener eff a = a -> WithRef eff Unit
type Unlistener eff = WithRef eff Unit

foreign import unsafeCoerce """
  function unsafeCoerce(x) {
    return x;
  }""":: forall a b. a -> b

foreign import unsafePerformEff """
  function unsafePerformEff(eff) {
    return eff();
  }""":: forall e a. Eff e a -> a

foreign import eqRef """
  function eqRef(r1) {
    return function(r2) {
      return r1 === r2;
    }
  }""" :: forall a. RefVal a -> RefVal a -> Boolean

instance eqRefVal :: Eq (RefVal a) where
  (==) = eqRef
  (/=) r1 r2 = not (r1 == r2)

removeOnce :: forall a. (a -> Boolean) -> [a] -> [a]
removeOnce _ [] = []
removeOnce p (x:xs) = if p x then xs else x : removeOnce p xs
