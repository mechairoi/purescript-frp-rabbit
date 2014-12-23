# Module Documentation

## Module FRP.Rabbit

## Module FRP.Rabbit.Handler

### Types

    type EventSignal a eff = Signal (WithRef eff) a

    type Handler a eff = a -> WithRef eff Unit


### Values

    createEventHandler :: forall a eff2 eff. WithRef eff { event :: EventSignal a eff2, handler :: Handler a eff2 }


## Module FRP.Rabbit.Signal

### Types

    newtype Signal m a where
      Signal :: ((a -> m Unit) -> m Unit) -> Signal m a

    type WithRef eff = Eff (ref :: Ref | eff)


### Type Class Instances

    instance altSignal :: (Monad m) => Alt (Signal m)

    instance applicativeSignal :: (Functor m, Monad m) => Applicative (Signal m)

    instance applySignal :: (Functor m, Monad m) => Apply (Signal m)

    instance bindSignal :: (Monad m) => Bind (Signal m)

    instance functorSignal :: (Monad m) => Functor (Signal m)

    instance monadSignal :: (Monad m) => Monad (Signal m)

    instance monadTransSignal :: MonadTrans Signal

    instance plusSignal :: (Monad m) => Plus (Signal m)

    instance semigroupSignal :: (Monad m) => Semigroup (Signal m a)


### Values

    runSignal :: forall m a. (Monad m) => Signal m a -> (a -> m Unit) -> m Unit

    stateful :: forall eff a b. (a -> b -> b) -> b -> Signal (WithRef eff) a -> Signal (WithRef eff) b



