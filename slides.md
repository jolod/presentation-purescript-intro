# Got.λ 2017-09-24 - Introduction to PureScript

Presented at Gothenburg's functional programming meetup group, [Got.λ](https://www.meetup.com/got-lambda/), by Johan Lodin.

## PureScript

"PureScript is a small strongly typed programming language that compiles to JavaScript."

* ML family
* Designed to target JavaScript
* Very explicit
* Loves abstraction

Created by Phil Freeman (@paf31)

Currently at version 0.11.6.

## Hello!

```purescript
import Prelude
import Control.Monad.Eff.Console (log)

main = do
    log "Hello, world!"
```

## Designed to be compiled to JavaScript

[try.purescript.org](http://try.purescript.org)

Show functions, ADTs, case, newtypes, `where` and `let`.

## Very close to Haskell

```text
== Syntax
== ADT
== Type classes
== Modules
```

```text
/= Strict (not lazy)
/= Records
/= Extensible effects
/= Rank-n types and explicit forall quantifier
/= Exceptions
```

## But not quite Haskell ...

Let's concatenate two strings.

```purescript
import Prelude          -- Prelude is explicit.
foobar = "foo" ++ "bar" -- Nope.
```

Let's search the docs!

## Hunting for string concatenation

...

## Haskell's type signatures

```purescript
(&&) :: Bool -> Bool -> Bool
(+)  :: Num a => a -> a -> a
(-)  :: Num a => a -> a -> a
(.)  :: (b -> c) -> (a -> b) -> a -> c
id   :: a -> a
```

## PureScript's type signatures

```purescript
(&&)  :: forall a. HeytingAlgebra a => a -> a -> a
(+)   :: forall a. Semiring a => a -> a -> a
(-)   :: forall a. Ring a => a -> a -> a
(<<<) :: forall b c d. Semigroupoid a => a c d -> a b c -> a b d
id    :: forall t. Category a => a t t
```

## Records

Records are JavaScript objects.

```purescript
point = {x: 3, y: 4} -- A record with x and y properties.
point.x              -- Get x.
point { x = 4 }      -- Set x.
```

[try.purescript.org](http://try.purescript.org)

### Extensible records

```purescript
dist2d :: forall rows. { x :: Number, y :: Number | rows } -> Number
dist2d point = Math.sqrt (point.x * point.x + point.y * point.y)

dist2d {x: 3}             -- Compilation error.
dist2d {x: 3, y: 4}       -- 5
dist2d {x: 3, y: 4, z: 1} -- 5
```

### Comment about extensibility

```purescript
type Point2D rows = { x :: Number, y :: Number | rows }
type Point3D rows = Point2D ( z :: Number | rows )
```

Proliferation of row variables for records containing records. (Compare with how maps are used in Clojure.)

### Exceptions (an effect)

```purescript
error :: String -> Error

throw :: forall eff a
       . String
      -> Eff (exception :: EXCEPTION | eff) a

try :: forall eff a
     . Eff (exception :: EXCEPTION | eff) a
    -> Eff eff (Either Error a)
```

### Merging records

```purescript
merge :: forall r s t. Union r s t => Record r -> Record s -> Record t

merge {a: 3, b: 7} {b: "new", c: "more"}
```

(Multiple labels are allowed. Don't ask.)

### Records and type classes

```purescript
newtype Point2D a = Point2D { x :: a, y :: a }

instance showPoint2D :: Show a => Show (Point2D a) where
show (Point2D p) = "[Point2D " <> show p.x <> " " <> show p.y <> "]"

instance functorPoint2D :: Functor Point2D where
map f (Point2D { x, y }) = Point2D {x: f x, y: f y}

instance applyPoint2D :: Apply Point2D where
apply (Point2D f) (Point2D p) = Point2D {x: f.x p.x, y: f.y p.y}

main = do
let p = Point2D {x: 3, y: 4}
    f = Point2D {x: (+) 2, y: (*) 3}
    p2 = f <*> p
log $ show p2
```

Output:

    [Point2D 5 12]

## Haskell-like lenses ("Profunctor lenses")

```purescript
import Data.Lens (Lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

-- Polymorphic record lens
foo :: forall a b r. Lens { foo :: a | r } { foo :: b | r } a b
foo = prop (SProxy :: SProxy "foo")
```

Example:

```purescript
foobar = {foo: 1, bar: "hello"}
over foo inc foobar -- {foo: 2, bar: "hello"}
```

## JavaScript FFI

PureScript to JavaScript:

```purescript
foreign import inc :: Number -> Number
```

... with multiple arguments:

```purescript
import Data.Function (Fn2)
import Data.Function.Uncurried (runFn2)

foreign import addImpl :: Fn2 Number Number Number

add :: Number -> Number -> Number
add = runFn2 addImpl
```

JavaScript to PureScript:

```javascript
YourModule.foo(arg1)(arg2)
YourModule.fooWithEffects(arg1)(arg2)()
```

# PureScript websocket server and client

(Demo)

## Hello, with a prompt on node.js

```purescript
module App.Main

import Prelude
import Control.Monad.Eff.Console (log)
import Node.ReadLine as RL

main = do
    log "What's your name?"
    interface <- RL.createConsoleInterface RL.noCompletion
    RL.setLineHandler interface $ \line -> do
    log ("Hello, " <> line <> "!")
    RL.close interface
```

In `start.js`:

```javascript
const App = require('./output/App.Main');

App.main();
```

## The End

Thank you!

github.com/jolod
