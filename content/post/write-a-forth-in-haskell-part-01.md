+++
title = "Write a Forth in Haskell: Part 01"
date = 2020-01-15T20:05:10+01:00
description = "In which the author has succesfully convinced you that implementing Forth is an acceptable reading pastime."
draft = false
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

<!--more-->

* [Introduction](/post/write-a-forth-in-haskell-intro/)
* [Part 01](/post/write-a-forth-in-haskell-part-01/) <-- 
* [Part 02](/post/write-a-forth-in-haskell-part-02/)
* [Part 03](/post/write-a-forth-in-haskell-part-03/)

## Bootstrapping

Let us create a cabal project:

```bash
$ cabal init --simple -p forth --cabal-version=3.4 \
           --language=GHC2021 --libandexe --tests \
           --test-dir=test forth
```

Now, let's open a new file, `src/StateMachine.hs`, and make it a module with the following line:

```haskell
module StateMachine where
```

Because we need a proper datatype to represent Forth's execution model, I chose to implement it
with a [Linked List](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html)-based stack.

```Haskell
import Data.List qualified as List

newtype Stack = Stack {getStack :: [Integer]}
  deriving newtype (Show, Eq)
```

What the above lines mean is that we create a _newtype_, a wrapper around
`[Integer]`, with a constructor named `Stack`, and an accessor function
called `getStack`.
The `deriving newtype` line allows us to inherit the `Show` and `Eq` instances 
of the underlying type instead of the wrapper's instances.

So what would an empty stack look like? Like this:

```haskell
initStack :: Stack
initStack = Stack List.empty
```

We now need a function that will “process”, so to speak, the different operations we receive from our input:

```Haskell
process :: Stack -> Text -> Stack
process stack "+" = add
process stack "-" = sub
process stack a   = push item stack
  where
    item = parseInt a
```

The {{% tooltip "`process`" "process :: Stack -> Text -> Stack" %}} function is a pure function that carries its state, the `stack` argument, with it.
We pattern-match on defined keywords and operators of the language, and call the
appropriate functions each time. The catch-all case at the bottom is there to grab numerical elements
from our input and simply push them on the stack, which we will then return.

That being said, we also need a couple of helpers.
The first one is {{% tooltip "`parseInt`" "parseInt :: Text -> Integer" %}}. We have encountered it in the above `where`-clause.  
The core of its definition relies on the {{% tooltip "`decimal`" "decimal :: Integral a => Either String (Integer, Text)"%}} function from [`Data.Text.Read`][Data.Text.Read],
as well as {{% tooltip "`pack`" "pack :: String -> Text" %}} from [`Data.Text`][Data.Text].

```Haskell
import Data.Text.Read (decimal)
import Data.Text (pack)

-- […]

parseInt :: Text -> Integer
parseInt a = either (error . pack) fst (decimal a)
```

However, this one-liner may be quite incomprehensible.
Here is how we can rewrite this function:

```Haskell
parseInt a = 
  case decimal a of
    Right result -> fst result
    Left  errorMsg -> error $ pack errorMsg
```

`decimal` has the following type: `Text -> Either String (Integer, Text)`,
which means it can either return an error message in its Left parameter (`String`),
or return a tuple of `(Integer, Text)` when parsing succeeds. The `Text` part of
the tuple is used if and only if the number you intend to parse is followed with non-numerical characters. In practice, this translates to:

```Haskell
λ❯ decimal "32ee" :: Either String (Integer, Text)
Right (32,"ee")
```

Hence the use of {{% tooltip "`fst`" "fst :: (a, b) -> a" %}} on that result.
We simply do not care about the second part, only about the integer.

---
 
*Now, you may be wondering about the use of {{% tooltip "`error`" "error :: String -> a"%}}.
It doesn't really return an integer, does it? Should it? Well, the thing about
this function is that it will return whatever type you ask of it, because it will stop the execution of the program.  
Terribly unsafe from a types perspective, morally digusting, but we are going to need it.*

---

At that point, here's what our file looks like:

```Haskell
module StateMachine where

import Data.Text (pack)
import Data.Text.Read (decimal)
import Data.List (List)
import qualified Data.List as List

newtype Stack = Stack {getStack :: [Integer]}
  deriving newtype (Show, Eq)

initStack :: Stack
initStack = Stack []

process :: Stack -> Text -> Stack
process stack "+" = add stack
process stack "-" = sub stack
process stack a  = push item stack
  where
    item = parseInt a

parseInt :: Text -> Integer
parseInt a = either (error . pack) fst (decimal a)
-- Equivalent to
-- parseInt = case parseInt a of
--           Right result   -> fst result
--           Left  errorMsg -> error $ pack errorMsg
-- 
-- either :: (a -> c) -> (b -> c) -> Either a b -> c
--            ^^^^^^      ^^^^^^     ^^^^^^^^^^
--              │            │            │  
--       this function   this function  The value
--       is called on    is called on   to be tested
--       the value in    the value in
--       the `Left`      the `Right`
```

## Addition, subtraction


The next step is to implement the basics of stack manipulation. 

The first function, `push`, is implemented as a `cons` operation:

```Haskell
push :: Integer -> Stack -> Stack
push item stack = item : stack
```

Its famous counterpart `pop` will not be implemented *yet*. 

Then, let's take care of addition:

```Haskell
add :: Stack -> Stack
add stack =
  if checkSize 2 stack
  then
    let (elems, newStack) = List.splitAt 2 (getStack stack)
        result = sum elems
     in push result (Stack newStack)
  else
    error "Stack underflow!"
```

Which brings us to our next helper: `checkSize`.

```Haskell
checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  (length (getStack stack)) >= requiredSize
```

Fundamentally, we need to be sure that the operation we make is safe at the
stack level. 

Now that we have all the cards, let's combine them.
First, with the help of {{% tooltip "`List.splitAt`" "Int -> [a] -> ([a], [a])" %}}, we grab a 2-tuple of lists. The first one supposedly contains
the first two elements, and the second one has the rest of the stack in it.

With the help of {{% tooltip "`checkSize`" "checkSize :: Int -> Stack -> Bool" %}}, we then make sure to only proceed to the actual sum if *and only if* the first list, `elems`, has two elements.
And finally, we push the result to the stack.

The subtraction function is similar in intent:

```Haskell
sub :: Stack -> Stack
sub stack =
  if checkSize 2 stack
  then
    let (elems, newStack) = List.splitAt 2 (getStack stack)
        result = sub' $ List.reverse elems
        sub' = List.foldl1 (-)
     in push result (Stack newStack)
  else
    error "Stack underflow!"
```

With the slight difference that we define our own subtraction function, and we reverse the list beforehand so we get a correct result.

`foldl1` iterates over a container and applies the supplied function (`(-)`) over those elements while keeping an accumulator. By convention, the `1` suffix tells us that
we do not need to supply a initial accumulator to the recursive function, assuming a non-empty container to start with.  

So far, we implemented addition and subtraction. Their definion were a bit convoluted,
unnessarily even, due to a lack of a better abstraction. But be patient.  
In [part 02](/post/write-a-forth-in-haskell-part-02), we will explore more traditional Forth operations, such as duplication, drop, and rotation, amongst others.

[Data.Text.Read]: https://hackage.haskell.org/package/text-2.1/docs/Data-Text-Read.html
[Data.Text]: https://hackage.haskell.org/package/text-2.1/docs/Data-Text.html
