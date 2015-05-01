# Module Documentation

## Module FRP.Rabbit

#### `WithRef`

``` purescript
type WithRef eff a = Eff (ref :: Ref | eff) a
```


#### `Sink`

``` purescript
type Sink eff a = a -> WithRef eff Unit
```

The `Sink a` type represents callback functions.

`Sink` is often used by `sinkR` and `sinkE`.
These function are register a callback.

#### `Event`

``` purescript
type Event eff a = Event.Event eff a
```

The `Event eff a` type represents streams of timed values (discrete siganl).
It's a primitive of `FRP.Rabbit`.

These timed values are inhabitants of _a_.
`Event` is an instance of `Monoid` and `Functor`.

#### `newEventWithSource`

``` purescript
newEventWithSource :: forall e a. WithRef e { source :: Sink e a, event :: Event e a }
```

The `newEventWithSource` function create new pair of an `Event` and a source.
The source function triggers a timed value for the event.

#### `sinkE`

``` purescript
sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
```

The `sinkE` function registers a callback function for an `Event`.

It's like `.addEventListener()` or subscribe an observable.
`sinkE` can only subscribe future values to prevent memory leak.
Please use `sinkR` to subscribe an `Behavior`.

#### `Behavior`

``` purescript
type Behavior e a = Behavior.Behavior e a
```

The `Behavior eff a` type represents streams of timed values,
but semantically it is continuous time function whose value is the last timed value at the thme.
It's a primitive of `FRP.Rabbit`.

`Behavior` has current value and an `Event` represents future values.
These timed values are inhabitants of _a_.
`Event` is an instance of `Monad`.

#### `sinkR`

``` purescript
sinkR :: forall e a. Sink e a -> Behavior e a -> WithRef e (WithRef e Unit)
```

The `sinkR` function registers a callback function for a `Behavior`.

It's like `.addEventListener()` or subscribe an observable.
`sinkR` can only subscribe current and future values to prevent memory leak.
Please use `sinkE` to subscribe an `Event`.

#### `stepperR`

``` purescript
stepperR :: forall a e. a -> Event e a -> Behavior e a
```


#### `switcherR`

``` purescript
switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
```


#### `stateful`

``` purescript
stateful :: forall e a b. (a -> b -> b) -> b -> Event e a -> WithRef e (Behavior e b)
```



## Module FRP.Rabbit.VirtualDOM

#### `runBehaviorVTree`

``` purescript
runBehaviorVTree :: forall e. Behavior (dom :: DOM | e) VTree -> WithRef (dom :: DOM | e) DOM.Node
```



## Module FRP.Rabbit.Internal.Behavior

#### `Behavior`

``` purescript
newtype Behavior e a
```


#### `sinkR`

``` purescript
sinkR :: forall e a. Sink e a -> Behavior e a -> WithRef e (WithRef e Unit)
```


#### `sinkRI`

``` purescript
sinkRI :: forall e a. SinkI e a -> Behavior e a -> WithRef e { unsink :: WithRef e Unit, after :: WithRef e Unit }
```


#### `functorBehavior`

``` purescript
instance functorBehavior :: Functor (Behavior e)
```


#### `applicativeBehavior`

``` purescript
instance applicativeBehavior :: Applicative (Behavior e)
```


#### `applyBehavior`

``` purescript
instance applyBehavior :: Apply (Behavior e)
```


#### `bindBehavior`

``` purescript
instance bindBehavior :: Bind (Behavior e)
```


#### `monadBehavior`

``` purescript
instance monadBehavior :: Monad (Behavior e)
```


#### `stepperR`

``` purescript
stepperR :: forall a e. a -> Event e a -> Behavior e a
```


#### `switcherR`

``` purescript
switcherR :: forall a e. Behavior e a -> Event e (Behavior e a) -> Behavior e a
```



## Module FRP.Rabbit.Internal.Event

#### `Event`

``` purescript
newtype Event e a
  = Event (ContT (Eff (ref :: Ref | e) Unit) (Eff (ref :: Ref | e)) a)
```


#### `sinkE`

``` purescript
sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
```

#### `sinkEI`

``` purescript
sinkEI :: forall e a. SinkI e a -> Event e a -> WithRef e (WithRef e Unit)
```

#### `newEventWithSource`

``` purescript
newEventWithSource :: forall e a. WithRef e { source :: Sink e a, event :: Event e a }
```


#### `monoidEvent`

``` purescript
instance monoidEvent :: Monoid (Event e a)
```


#### `functorEvent`

``` purescript
instance functorEvent :: Functor (Event e)
```


#### `semigroupEvent`

``` purescript
instance semigroupEvent :: Semigroup (Event e a)
```



## Module FRP.Rabbit.Internal.Sugar

#### `stateful`

``` purescript
stateful :: forall e a b. (a -> b -> b) -> b -> Event e a -> WithRef e (Behavior e b)
```



## Module FRP.Rabbit.Internal.Util

#### `WithRef`

``` purescript
type WithRef eff a = Eff (ref :: Ref | eff) a
```


#### `SinkI`

``` purescript
type SinkI eff a = a -> WithRef eff (WithRef eff Unit)
```


#### `Sink`

``` purescript
type Sink eff a = a -> WithRef eff Unit
```


#### `unsafeCoerce`

``` purescript
unsafeCoerce :: forall a b. a -> b
```


#### `unsafePerformEff`

``` purescript
unsafePerformEff :: forall e a. Eff e a -> a
```


#### `eqRef`

``` purescript
eqRef :: forall a. RefVal a -> RefVal a -> Boolean
```


#### `eqRefVal`

``` purescript
instance eqRefVal :: Eq (RefVal a)
```


#### `removeOnce`

``` purescript
removeOnce :: forall a. (a -> Boolean) -> [a] -> [a]
```




