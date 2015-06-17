# Module Documentation

## Module FRP.Rabbit

#### `Event`

``` purescript
type Event eff a = Event.Event eff a
```

A stream of events (discrete siganl).

These timed values are inhabitants of _a_.
`Event` is an instance of `Monoid` and `Functor`.

#### `Behavior`

``` purescript
type Behavior e a = Behavior.Behavior e a
```

A s time-varying value.
It semantically represents continuous time function whose value is the last timed value at the time.

`Behavior` has current value and an `Event` represents future values.
These timed values are inhabitants of _a_.
`Event` is an instance of `Monad`.

#### `newEvent`

``` purescript
newEvent :: forall e a. Eff (ref :: Ref | e) { push :: a -> Eff (ref :: Ref | e) Unit, event :: Event e a }
```

Returns an event, and a push action for pushing a value into the event.

#### `newBehavior`

``` purescript
newBehavior :: forall e a. a -> Eff (ref :: Ref | e) { push :: a -> Eff (ref :: Ref | e) Unit, behavior :: Behavior e a }
```

Create a new Behavior along with an action to push changes into it.
The returning behavior is recommended to `retainB`.

#### `listen`

``` purescript
listen :: forall e a. Event e a -> (a -> Eff (ref :: Ref | e) Unit) -> Eff (ref :: Ref | e) (Eff (ref :: Ref | e) Unit)
```

Listen for firings of this event. The returned `Eff _ Unit` is an Eff action that unregisters the listener. This is the observer pattern.
To listen to a Behavior use `listen (value b)` handler or `listen (updates b)` handler
`listen` function can subscribe only future values to prevent memory leak.

#### `never`

``` purescript
never :: forall e a. Event e a
```

An event that never fires.

#### `merge`

``` purescript
merge :: forall e a. Event e a -> Event e a -> Event e a
```

Merge two streams of events of the same type.
In the case where two event occurrences are simultaneous, both will be delivered.

#### `filterJust`

``` purescript
filterJust :: forall e a. Event e (Maybe a) -> Event e a
```

Unwrap Just values, and discard event occurrences with Nothing values.

#### `hold`

``` purescript
hold :: forall e a. a -> Event e a -> Eff (ref :: Ref | e) (Behavior e a)
```

Create a behavior with the specified initial value, that gets
updated by the values coming through the event.
The returning behavior is recommended to `retainB`.

#### `updates`

``` purescript
updates :: forall e a. Behavior e a -> Event e a
```

An event that gives the updates for the behavior.

#### `value`

``` purescript
value :: forall e a. Behavior e a -> Event e a
```

An event that gives the current value and the updates for the behavior.

#### `snapshot`

``` purescript
snapshot :: forall e a b c. (a -> b -> c) -> Event e a -> Behavior e b -> Event e c
```

Sample the behavior at the time of the event firing.

#### `switchE`

``` purescript
switchE :: forall e a. Behavior e (Event e a) -> Event e a
```

Unwrap an event inside a behavior to give a time-varying event implementation.

#### `switch`

``` purescript
switch :: forall e a. Behavior e (Behavior e a) -> Eff (ref :: Ref | e) (Behavior e a)
```

Unwrap a behavior inside another behavior to give a time-varying behavior implementation.

#### `sample`

``` purescript
sample :: forall e a. Behavior e a -> Eff (ref :: Ref | e) a
```

Obtain the current value of a behavior.

#### `once`

``` purescript
once :: forall e a. Event e a -> Event e a
```

Throw away all event occurrences except for the first one.

#### `filterE`

``` purescript
filterE :: forall e a. (a -> Boolean) -> Event e a -> Event e a
```

Only keep event occurrences for which the predicate is true.

#### `gate`

``` purescript
gate :: forall e a. Event e a -> Behavior e Boolean -> Event e a
```

Let event occurrences through only when the behavior's value is True.

#### `collectE`

``` purescript
collectE :: forall e a b. (a -> b -> b) -> b -> Event e a -> Eff (ref :: Ref | e) (Behavior e b)
```

Transform an event with a generalized state loop (a mealy machine). The function is passed the input and the old state and returns the new state and output value
The returning behavior is recommended to `retainB`.

#### `collect`

``` purescript
collect :: forall e a s. (a -> s -> s) -> s -> Behavior e a -> Eff (ref :: Ref | e) (Behavior e s)
```

#### `accum`

``` purescript
accum :: forall e a. a -> Event e (a -> a) -> Eff (ref :: Ref | e) (Behavior e a)
```

Accumulate state changes given in the input event.
The returning behavior is recommended to `retainB`.

#### `executeEff`

``` purescript
executeEff :: forall e a. Event e (Eff (ref :: Ref | e) a) -> Eff (ref :: Ref | e) (Event e a)
```

Execute the specified IO operation synchronously and fire the output event.

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

Cache the event occurrences and fire listeners.
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




