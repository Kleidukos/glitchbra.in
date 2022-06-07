+++
title = "Write a Forth in Haskell: Part 03"
date = 2020-02-15T15:25:00+01:00
description = "In which the author has succesfully convinced you that implementing Forth is an acceptable reading pastime."
toc = false
categories = ["Tutorials"]
tags = ["Forth", "Haskell", "PL"]
images = [
  "https://thedevs.network/static/img/posts/the-forth-programming-language.jpg"
] # overrides site-wide open graph image
[[copyright]]
  owner = "Hécate"
  date = "2020"
  license = "cc-by-nd-4.0"
+++

* [Introduction](/post/write-a-forth-in-haskell-intro/)
* [Part 01](/post/write-a-forth-in-haskell-part-01/)
* [Part 02](/post/write-a-forth-in-haskell-part-02/)
* [Part 03](/post/write-a-forth-in-haskell-part-03/) <-

Up until now, the execution of the interpreter was fairly straightforward.  
We could fold over a string of instructions, applying our `process` function on each token.

```Haskell
initStack :: Stack

processBuffer :: Text -> Stack
processBuffer buffer = foldl' process initStack (words buffer)
                                │       │          └─── Input
                                │       └─── Initial state
                                └─── Business logic
```

But something isn't quite right. Yes, we have implemented some stack operations, but there is a world beyond those.

In this part and the next, we will refactor the token processor from parts one and two, into an interpreter allowing operations on state, as well as
IO capabilities.

## Having a Monad Stack

Operating in a Monad is pretty awesome. You get access to cool functions like `get` and `put` if you are in the
[`State Monad`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Class.html#t:MonadState), or even proper error
management mechanisms that do not rely on runtime exceptions, in the case of [`MonadError`](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html)
But what is better than one monad? Several of them. Hence the expression "Monad Stack". You stack them on top of each-other, so you can enjoy their
capabilities in the same place.
In our case, we are interested in two things: state management, and IO. And by IO I mean "being able to output strings", because right now we don't
need more.

But Monad Stacks can sometimes be inconvenient. In our case, our stack would look like that:

```Haskell
StateT [Text] IO
```

And while there is nothing inherently wrong with this, the order in which we write the components of this stack matters, so when refactoring our program,
we would have to change the order in which they appear, as it would have an impact on how we would access those monad's features.

That is why we are going to use the MTL style of writing monad stacks.

### MTL to the rescue!

MTL is the “Monad Transformers Library”. Sitting on top of its sibling [`transformers`](https://hackage.haskell.org/package/transformers),
it provides typeclasses that we can use to declare the shape of our monad stack, without having to think about the order in which we write its type signature.

As you (probably) know, a program that runs in IO while returning nothing has the following type signature:

```haskell
returningNothing :: IO ()
```

Now, with the MTL style, our type signature would look like that:

```haskell
returningNothing :: (MonadIO m) => m ()
```

Nothing really changes in terms of execution, but now we gain some control over the nature of the monad in which we operate.  

### Combining State and IO

I will go straight to the point : This will be no more complicated than :

```haskell
MonadState AppState m, MonadIO m
```

We combine the State Monad (containing our application state), and the IO Monad.  
This enables us to write such functions:

```haskell
add :: (MonadIO m, MonadState AppState m) => m ()
add = do
  element1 <- pop
  element2 <- pop
  push (element1 + element2)
```

_(Don't mind the function body for now, we still have some stuff to cover before attaining this level of care-free declarative programming)_  

Operating in a combination of `MonadState` and `MonadIO` allows us to use IO when needed for side operations like on-disk logging if you want to
maintain a history of the operations, or even launching nukes on the side.


### Building our state

Our `AppState` will be quite basic, and will grow as we add more features to the interpreter:

```haskell
data AppState = AppState { buffer :: [Text]
                         , stack  :: Stack
                         } deriving (Show)
```

`buffer` will contain the forth instructions that are given to the interpreter, and `stack` will be the stack we're operating on.

We can put this definitions and the other ones in a separates `Types.hs` files:

```haskell
-- src/Types.hs
module Types where

import           Data.Vector     (Vector)
import qualified Data.Vector     as V

newtype Stack = Stack {getStack :: Vector Integer}
  deriving newtype (Show, Eq)

data AppState = AppState { buffer :: [Text]
                         , stack  :: Stack
                         } deriving (Show)

initStack :: Stack
initStack = Stack V.empty
```

## Stack operations

Now that we have the state available from the functions that require it,
it has become possible for us to query it and modify it in a declarative way.

So, let us rework the functions we have implemented in [part 01](/post/write-a-forth-in-haskell-part-01/),
starting with `add` and `sub`. In the classical Forth fashion, the addition is composed of two `pop`s and a `push`.

A bit like this, actually:

```Haskell
add :: (MonadState AppState m) => m ()
add = do
  element1 <- pop
  element2 <- pop
  push (element1 + element2)
```

`sub` is also much more straightforward:

```Haskell
sub :: (MonadState AppState m) => m ()
sub = do
  element1 <- pop
  element2 <- pop
  push (element2 - element1)
```

And that's it. But what of `dup`, `drop` and all the friends we made along the way?  
Well they're coming with us.

```haskell
dup :: (MonadIO m, MonadState AppState m) => m ()
dup = do
  element <- pop
  push element
  push element

drop :: (MonadIO m, MonadState AppState m) => m ()
drop = do
  _ <- pop
  pure ()

swap :: (MonadIO m, MonadState AppState m) => m ()
swap = do
  element1 <- pop
  element2 <- pop
  push element1
  push element2

over :: (MonadIO m, MonadState AppState m) => m ()
over = do
  element1 <- pop
  element2 <- pop
  push element2
  push element1
  push element2

rot :: (MonadIO m, MonadState AppState m) => m ()
rot = do
  element1 <- pop
  element2 <- pop
  element3 <- pop
  push element2
  push element1
  push element3
```

## Let's get poppin'!

Finally. Clean, declarative code to express our operations on the stack.  
Gone are the size checks on the stack and the manual splitting!
You can sweep all the nasty, vector-manipulating code we wrote earlier under
the rug, because this Dark Age is over. Or is it?

We cannot entirely get rid of "low-level" operations on Vectors.
As Fred Hébert says so well, [complexity has to live somwhere](https://ferd.ca/complexity-has-to-live-somewhere.html).

We can't forget it's there but we can manage to centralise it, and this is exactly
what we are going to do. In this new version, `push` will make us of the 
{{% tooltip "`modify`" "modify :: MonadState s m => (s -> s) -> m ()" %}} function from
[MonadState](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Class.html#v:modify-39-). We pass it a callback whose first argument is
the current state and returns the new state.

To avoid burdening the reading of the function, a new helper is defined:

```Haskell
addToStack :: Integer -> Stack -> Stack
addToStack element stack = under (V.cons element) stack
```

This one makes use of the `under` helper, brought to us by Relude.
As said at the begining of the series, Relude is an alternative prelude
that fixes a lot of the inherent issues that people have with the standard Prelude,
*and* brings its lot of new functions. One of them is
{{% tooltip "`under`" "under :: forall n a . Coercible a n => (n -> n) -> (a -> a)" %}}
Through the clever use of {{% tooltip "`coerce`" "coerce :: Coercible a b => a -> b"%}},
the function allows us to make a transformation on the value wrapped by a newtype.
And in our case, the transformation is the concatenation of an element to the stack.

But enough with the explanations, here is the code!: 

```Haskell
push :: (MonadState AppState m) => Integer -> m ()
push newElement =
  modify' (\state ->
    let newStack = addToStack newElement (stack state)
     in state {stack = newStack})
```

Its companion `pop` does a bit more, though. This is the place where
bound checking happens, and where `error` may be used:

```Haskell
pop :: (MonadState AppState m) => m Integer
pop = do
  stack' <- gets stack
  if (checkSize 1 stack')
  then do
    let (hd, tl) = V.splitAt 1 (getStack stack')
    updateStack (Stack tl)
    pure $ V.head hd
  else
    error "Stack underflow!"
```

It could have been written in a more "compositional" style, but control flow is
the main point of this function, so the `if/then/else` construct is appropriate here.
To ease reading, two helpers are defined:

```Haskell
updateStack :: (MonadState AppState m) => Stack -> m ()
updateStack newStack = do
  oldState <- get
  put $ oldState{stack=newStack}
```

Its name is quite explicit: We pass in a `Stack`, and the function will update our state with it.  


```haskell
checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  (length $ getStack stack) >= requiredSize
```

This provides a predicate that checks whether or not our stack is full enough
to allow operand to be applied.

## Outputting elements

Empowered, and emboldened, by this addition to our collection, we can use our
ability to do IO in controlled places to implement three new functions:

* `period` <- outputs the top of the stack on stdout
* `emit`   <- outputs the ASCII character for the element on top of the stack
* `cr`     <- outputs a newline

Like what we wrote before, we are able to express those operations with the
building blocks of stack manipulation.

```Haskell
period :: (MonadIO m, MonadState AppState m) => m ()
period = do
  element <- pop
  putStr $ (show element) <> " " -- Yes, we *do* want to add a space after each element
```

`emit` is less straightforward since you need to know how to convert from an integer
to an ASCII char. Thankfully, {{% tooltip "`Data.Char.chr`" "chr :: Int -> Char" %}} has
our back. We can convert the ASCII code to a character.

However, `chr` takes an `Int`, and we have a Vector full of… `Integer`s! Which
are not the same thing! Whereas `Integer` is an arbitrary-precision number
(also called “bigint” in some other languages), `Int` is a signed 64-bit integer.
There must be a conversion step from one to the other. And there is.

```Haskell
fromInteger :: Integer -> a
```

The documentation says “Conversion from an Integer”, so that must be it.

Let's try it: 

```Haskell
λ❯ fromInteger (3 :: Integer) :: Int
3
it :: Int
```
And that is indeed what we want.

Here is how we are going to do it:

1. We get the top of the stack, with `pop`, and inline the conversion
   to an `Int` with {{% tooltip "`fromInteger`" "fromInteger :: Num a => Integer -> a" %}} and `(<$>)`, which is the infix version of `fmap`.  
2. Then, in just one step, we convert the number into an ASCII character,
   and print it without adding a newline. This requires a bit of plumbing
   due to the fact that we are operating in a Monad `m` that is
   constrained on `MonadIO`, and not directly on `IO`. Hence the usage
   of {{% tooltip "`liftIO`" "liftIO :: MonadIO m => IO a -> m a" %}} to achieve
   such as thing:

```Haskell
emit :: (MonadIO m, MonadState AppState m) => m ()
emit = do
  element <- fromInteger <$> pop
  liftIO $ putChar (chr element)
```

And finally, `cr`, short for [carriage return](https://en.wikipedia.org/wiki/Carriage_return),
shall be implemented as such:

```Haskell
cr :: (MonadIO m) => m ()
cr = putStr "\n"
```
