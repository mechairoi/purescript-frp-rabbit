# Module Documentation

## Module FRP.Rabbit

#### `listen`

``` purescript
listen :: forall e a. Event e a -> (a -> Eff (ref :: Ref | e) Unit) -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
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
newEvent :: forall e a. Eff (ref :: Ref | e) { push :: a -> Eff (ref :: Ref | e) Unit, event :: Event e a }
```

The `newEvent` function create new pair of an `Event` and a push function.
The push function triggers a timed value for the event.

#### `newBehavior`

``` purescript
newBehavior :: forall e a. a -> Eff (ref :: Ref | e) { push :: a -> Eff (ref :: Ref | e) Unit, behavior :: Behavior e a }
```

The returning behavior is recommended to `retainB`.

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
hold :: forall e a. a -> Event e a -> Eff (ref :: Ref | e) (Behavior e a)
```


#### `updates`

``` purescript
updates :: forall e a. Behavior e a -> Event e a
```


#### `value`

``` purescript
value :: forall e a. Behavior e a -> Event e a
```


#### `snapshot`

``` purescript
snapshot :: forall e a b c. (a -> b -> c) -> Event e a -> Behavior e b -> Event e c
```


#### `switchE`

``` purescript
switchE :: forall e a. Behavior e (Event e a) -> Event e a
```


#### `switch`

``` purescript
switch :: forall e a. Behavior e (Behavior e a) -> Eff (ref :: Ref | e) (Behavior e a)
```


#### `sample`

``` purescript
sample :: forall e a. Behavior e a -> Eff (ref :: Ref | e) a
```


#### `once`

``` purescript
once :: forall e a. Event e a -> Event e a
```


#### `filterE`

``` purescript
filterE :: forall e a. (a -> Boolean) -> Event e a -> Event e a
```


#### `gate`

``` purescript
gate :: forall e a. Event e a -> Behavior e Boolean -> Event e a
```


#### `collectE`

``` purescript
collectE :: forall e a b. (a -> b -> b) -> b -> Event e a -> Eff (ref :: Ref | e) (Behavior e b)
```

The returning behavior is recommended to `retainB`.

#### `collect`

``` purescript
collect :: forall e a s. (a -> s -> s) -> s -> Behavior e a -> Eff (ref :: Ref | e) (Behavior e s)
```

The returning behavior is recommended to `retainB`.

#### `accum`

``` purescript
accum :: forall e a. a -> Event e (a -> a) -> Eff (ref :: Ref | e) (Behavior e a)
```

The returning behavior is recommended to `retainB`.

#### `retain`

``` purescript
retain :: forall e a. Event e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
```

Keep the event as active.
The `retain` function returns the function to release.

JavaScript has no weak reference. So we have to manually manage activity
of `Event`s. To prevent memory leak, `Event`s are activated only if
one or more listeners are exist (like reference counting). This function
simply registers a dummy no-op lisetener.

#### `cache`

``` purescript
cache :: forall e a. Event e a -> Eff (ref :: Ref | e) (Event e a)
```

The returning event is recommended to `retain`.

#### `retainB`

``` purescript
retainB :: forall e a. Behavior e a -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
```

Keep the `Behavior` as active.
The `retainB` function returns the function to release.

Same as `retain` except for Behaviors.


## Module FRP.Rabbit.Class

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



## Module FRP.Rabbit.VirtualDOM

#### `runBehaviorVTree`

``` purescript
runBehaviorVTree :: forall e. Behavior (dom :: DOM | e) VTree -> WithRef (dom :: DOM | e) DOM.Node
```




