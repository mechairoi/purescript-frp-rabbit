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


#### `Event`

``` purescript
type Event = Event.Event
```


#### `Reactive`

``` purescript
type Reactive = Reactive.Reactive
```



## Module FRP.Rabbit.Event

#### `Event`

``` purescript
data Event e a
```


#### `newEventWithSource`

``` purescript
newEventWithSource :: forall e a. WithRef e { source :: Sink e a, event :: Event e a }
```


#### `sinkE`

``` purescript
sinkE :: forall e a. Sink e a -> Event e a -> WithRef e (WithRef e Unit)
```


#### `sinkEI`

``` purescript
sinkEI :: forall e a. SinkI e a -> Event e a -> WithRef e (WithRef e Unit)
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



## Module FRP.Rabbit.Reactive

#### `Reactive`

``` purescript
data Reactive e a
```


#### `sinkR`

``` purescript
sinkR :: forall e a. Sink e a -> Reactive e a -> WithRef e (WithRef e Unit)
```


#### `sinkRI`

``` purescript
sinkRI :: forall e a. SinkI e a -> Reactive e a -> WithRef e { unreg :: WithRef e Unit, after :: WithRef e Unit }
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
stepperR :: forall a eff. a -> Event eff a -> Reactive eff a
```


#### `switcherR`

``` purescript
switcherR :: forall a eff. Reactive eff a -> Event eff (Reactive eff a) -> Reactive eff a
```



## Module FRP.Rabbit.Sugar

#### `stateful`

``` purescript
stateful :: forall e a b. (a -> b -> b) -> b -> Event e a -> WithRef e (Reactive e b)
```


## Module FRP.Rabbit.VirtualDOM

#### `runReactiveVTree`

``` purescript
runReactiveVTree :: forall e. Reactive (dom :: DOM | e) VTree -> WithRef (dom :: DOM | e) DOM.Node
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




