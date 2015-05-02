module FRP.Rabbit.Internal.Reactive
  ( Reactive(..)
  , sync
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref

newtype Reactive e a = Reactive (Eff (ref :: Ref | e) a)

sync :: forall e a. Reactive e a -> Eff (ref :: Ref | e) a
sync (Reactive eff) = eff

instance functorReactive :: Functor (Reactive e) where
  (<$>) g (Reactive fa) = Reactive $ g <$> fa

instance applicativeReactive :: Applicative (Reactive e) where
  pure a = Reactive $ pure a

instance applyReactive :: Apply (Reactive e) where
  (<*>) (Reactive uf) (Reactive ua) = Reactive $ uf <*> ua

instance bindReactive :: Bind (Reactive e) where
  (>>=) (Reactive ma) k = Reactive $ ma >>= (sync <<< k)

instance monadReactive :: Monad (Reactive e)

-- instance monadEffReactive :: MonadEff (Reactive e) where
--   liftEff = Reactive
