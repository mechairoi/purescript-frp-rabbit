# Module Documentation

## Module FRP.Rabbit

#### `Reactive`

``` purescript
type Reactive eff a = Reactive.Reactive eff a
```


#### `sync`

``` purescript
sync :: forall e a. Reactive e a -> Eff (ref :: Ref | e) a
```


#### `listen`

``` purescript
listen :: forall e a. Event e a -> (a -> Eff (ref :: Ref | e) Unit) -> Reactive e (Eff (ref :: Ref | e) Unit)
```

The `listen` function registers a callback function for an `Event`.

It's like `.addEventListener()` or subscribe an observable.
`listen` can only subscribe future values to prevent memory leak.

#### `Event`

``` purescript
type Event eff a = Event.Event eff a
```

The `Event eff a` type represents streams of timed values (discrete siganl).
It's a primitive of `FRP.Rabbit`.

These timed values are inhabitants of _a_.
`Event` is an instance of `Monoid` and `Functor`.

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

#### `newEvent`

``` purescript
newEvent :: forall e a. Reactive e { push :: a -> Reactive e Unit, event :: Event e a }
```

The `newEvent` function create new pair of an `Event` and a push function.
The push function triggers a timed value for the event.

#### `never`

``` purescript
never :: forall e a. Event e a
```


#### `merge`

``` purescript
merge :: forall e a. Event e a -> Event e a -> Event e a
```


#### `filterJust`

``` purescript
filterJust :: forall e a. Event e (Maybe a) -> Event e a
```


#### `hold`

``` purescript
hold :: forall e a. a -> Event e a -> Reactive e (Behavior e a)
```


#### `updates`

``` purescript
updates :: forall e a. Behavior e a -> Event e a
```


#### `value`

``` purescript
value :: forall e a. Behavior e a -> Event e a
```


#### `sample`

``` purescript
sample :: forall e a. Behavior e a -> Reactive e a
```

#### `collectE`

``` purescript
collectE :: forall e a b. (a -> b -> b) -> b -> Event e a -> Reactive e (Behavior e b)
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


#### `value`

``` purescript
value :: forall e a. Behavior e a -> Event e a
```


#### `updates`

``` purescript
updates :: forall e a. Behavior e a -> Event e a
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


#### `sample`

``` purescript
sample :: forall e a. Behavior e a -> Reactive e a
```


#### `monadBehavior`

``` purescript
instance monadBehavior :: Monad (Behavior e)
```


#### `hold`

``` purescript
hold :: forall a e. a -> Event e a -> Reactive e (Behavior e a)
```



## Module FRP.Rabbit.Internal.Event

#### `Event`

``` purescript
newtype Event e a
  = Event (ContT (Eff (ref :: Ref | e) Unit) (Eff (ref :: Ref | e)) a)
```


#### `listen`

``` purescript
listen :: forall e a. Event e a -> Listener e a -> Reactive e (Unlistener e)
```

#### `listenI`

``` purescript
listenI :: forall e a. Event e a -> ListenerI e a -> Reactive e (Unlistener e)
```

#### `newEvent`

``` purescript
newEvent :: forall e a. Reactive e { push :: a -> Reactive e Unit, event :: Event e a }
```


#### `monoidEvent`

``` purescript
instance monoidEvent :: Monoid (Event e a)
```


#### `never`

``` purescript
never :: forall e a. Event e a
```


#### `functorEvent`

``` purescript
instance functorEvent :: Functor (Event e)
```


#### `semigroupEvent`

``` purescript
instance semigroupEvent :: Semigroup (Event e a)
```


#### `merge`

``` purescript
merge :: forall e a. Event e a -> Event e a -> Event e a
```


#### `filterJust`

``` purescript
filterJust :: forall e a. Event e (Maybe a) -> Event e a
```



## Module FRP.Rabbit.Internal.Reactive

#### `Reactive`

``` purescript
newtype Reactive e a
  = Reactive (Eff (ref :: Ref | e) a)
```


#### `sync`

``` purescript
sync :: forall e a. Reactive e a -> Eff (ref :: Ref | e) a
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



## Module FRP.Rabbit.Internal.Sugar

#### `collectE`

``` purescript
collectE :: forall e a b. (a -> b -> b) -> b -> Event e a -> Reactive e (Behavior e b)
```



## Module FRP.Rabbit.Internal.Util

#### `WithRef`

``` purescript
type WithRef eff a = Eff (ref :: Ref | eff) a
```


#### `ListenerI`

``` purescript
type ListenerI eff a = a -> WithRef eff (WithRef eff Unit)
```


#### `Listener`

``` purescript
type Listener eff a = a -> WithRef eff Unit
```


#### `Unlistener`

``` purescript
type Unlistener eff = WithRef eff Unit
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




