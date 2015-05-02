module FRP.Rabbit.Internal.Reactive
  ( Reactive(..)
  , ReactiveR(..)
  , sync
  , liftR
  , sequenceR
  , mapAfter
  ) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref (Ref())
import Data.Traversable (sequence)
import Data.Foldable (sequence_)

-- XXX WriterT?
newtype Reactive e a = Reactive (Eff e { r :: a, after :: Eff e Unit } )

type ReactiveR e a = Reactive (ref :: Ref | e) a

unReactive :: forall e a. Reactive e a
           -> Eff e { r :: a, after :: Eff e Unit }
unReactive (Reactive r) = r

sync :: forall e a. Reactive e a -> Eff e a
sync (Reactive eff) = do
  x <- eff
  x.after
  pure x.r

mapAfter :: forall e a. (Eff e Unit -> Eff e Unit) -> Reactive e a -> Reactive e a
mapAfter f r = Reactive $ do
  o <- unReactive r
  pure $ o { after = f o.after }

instance functorReactive :: Functor (Reactive e) where
  (<$>) g (Reactive fa) = Reactive $ do
    a <- fa
    pure { r: g a.r, after: a.after }

instance applicativeReactive :: Applicative (Reactive e) where
  pure a = Reactive $ pure { r: a, after: pure unit }

instance applyReactive :: Apply (Reactive e) where
  (<*>) (Reactive uf) (Reactive ua) = Reactive $ do
    xf <- uf
    xa <- ua
    pure { r: xf.r xa.r
         , after: do
             xf.after
             xa.after }

instance bindReactive :: Bind (Reactive e) where
  (>>=) (Reactive ma) k = Reactive $ do
    xa <- ma
    xb <- unReactive $ k xa.r
    pure { r: xb.r
         , after: do
             xa.after
             xb.after }

instance monadReactive :: Monad (Reactive e)

liftR eff = Reactive $ do -- XXX rename to liftR?
    a <- eff
    pure { r: a, after: pure unit }

sequenceR :: forall e a. [Reactive e a] -> Reactive e [a]
sequenceR rs = Reactive $ do
  xs <- sequence $ unReactive <$> rs
  pure { r: _.r <$> xs, after: sequence_ $ _.after <$> xs }
