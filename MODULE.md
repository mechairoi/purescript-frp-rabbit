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
Please use `sinkR` to subscribe an `Reactive`.

#### `Reactive`

``` purescript
type Reactive e a = Reactive.Reactive e a
```

The `Reactive eff a` type represents streams of timed values,
but semantically it is continuous time function whose value is the last timed value at the thme.
It's a primitive of `FRP.Rabbit`.

`Reactive` has current value and an `Event` represents future values.
These timed values are inhabitants of _a_.
`Event` is an instance of `Monad`.

#### `sinkR`

``` purescript
sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
```

The `sinkR` function registers a callback function for a `Reactive`.

It's like `.addEventListener()` or subscribe an observable.
`sinkR` can only subscribe current and future values to prevent memory leak.
Please use `sinkE` to subscribe an `Event`.

#### `stepperR`

``` purescript
stepperR :: forall a e. a -> Event e a -> Reactive e a
```


#### `switcherR`

``` purescript
switcherR :: forall a e. Reactive e a -> Event e (Reactive e a) -> Reactive e a
```


#### `stateful`

``` purescript
stateful :: forall e a b. (a -> b -> b) -> b -> Event e a -> WithRef e (Reactive e b)
```



## Module FRP.Rabbit.VirtualDOM

#### `runReactiveVTree`

``` purescript
runReactiveVTree :: forall e. Reactive (dom :: DOM | e) VTree -> WithRef (dom :: DOM | e) DOM.Node
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



## Module FRP.Rabbit.Internal.Reactive

#### `Reactive`

``` purescript
newtype Reactive e a
```


#### `sinkR`

``` purescript
sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
```


#### `sinkRI`

``` purescript
sinkRI :: forall e a. SinkI e a -> Reactive e a -> WithRef e { unsink :: WithRef e Unit, after :: WithRef e Unit }
```


#### `functorReactive`

``` purescript
instance functorReactive :: Functor (Reactive e)
```


#### `applicativeReactive`

``` purescript
instance applicativeReactive :: Applicative (Reactive e)
```


#### `applyReactive`

``` purescript
instance applyReactive :: Apply (Reactive e)
```


#### `bindReactive`

``` purescript
instance bindReactive :: Bind (Reactive e)
```


#### `monadReactive`

``` purescript
instance monadReactive :: Monad (Reactive e)
```


#### `stepperR`

``` purescript
stepperR :: forall a e. a -> Event e a -> Reactive e a
```


#### `switcherR`

``` purescript
switcherR :: forall a e. Reactive e a -> Event e (Reactive e a) -> Reactive e a
```



## Module FRP.Rabbit.Internal.Sugar

#### `stateful`

``` purescript
stateful :: forall e a b. (a -> b -> b) -> b -> Event e a -> WithRef e (Reactive e b)
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




