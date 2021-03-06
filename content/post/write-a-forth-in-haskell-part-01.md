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

First, let's bootstrap a [stack](https://docs.haskellstack.org/en/stable/README/) project.
I am using my custom stack template, [ultimaskell](/ultimaskell.hsfiles), which uses the alternative prelude [Relude](https://kowainik.github.io/projects/relude), by the fantastic people at Kowainik, as well as some convenient language extensions.

```bash
stack new farth https://glitchbra.in/ultimaskell.hsfiles
```

Now, let's open a new file, `src/StateMachine.hs`, and make it a module with the following line:

```haskell
module StateMachine where
```

Because we need a proper datatype to represent Forth's execution model, I chose to implement it
with a [Vector](https://hackage.haskell.org/package/vector)-based stack.

```Haskell
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Stack = Stack {getStack :: Vector Integer}
  deriving newtype (Show, Eq)
```

What the last lines means is that we create a new type, a wrapper around
`Vector Integer`, with a constructor aptly named `Stack`, and an accessor function
called `getStack`.
The `deriving newtype` line allows us to have a `Show` and `Eq` instance that
will use the underlying type (in our case, `Vector Integer`) instead of the wrapper.

So what would an empty stack look like? Like this:

```haskell
initStack :: Stack
initStack = Stack V.empty
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
The core of its definition relies on the {{% tooltip "`decimal`" "decimal :: Integral a => Either String (Integer, Text)"%}} function from `Data.Text.Read`,
as well as {{% tooltip "`pack`" "pack :: String -> Text" %}} from `Data.Text`.

```Haskell
import Data.Text.Read (decimal)
import Data.Text (pack)

-- […]

parseInt :: Text -> Integer
parseInt a = either (error . pack) fst (decimal a)
```

However, this one-liner may be quite incomprehensible.
Here is how we can write this function:

```Haskell
parseInt a = 
  case decimal a of
    Right result   -> fst result
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

module StateMachine (
  module StateMachine -- We export the whole module
    )

import Data.Text (pack)
import Data.Text.Read (decimal)
import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Stack = Stack {getStack :: Vector Integer}
  deriving newtype (Show, Eq)

initStack :: Stack
initStack = Stack V.empty

process :: Stack -> Text -> Stack
process stack "+" = add stack
process stack "-" = sub stack
process stack a   = push item stack
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
--       the Left        the Right
```

## Addition, subtraction


The next step is to implement the basics of stack manipulation. 

The first function, `push`, is implemented as a `cons` operation:

```Haskell
push :: Integer -> Stack -> Stack
push item stack = V.cons item stack
```

Its famous counterpart `pop` will not be implemented *yet*. 

Then, let's take care of addition:

```Haskell
add :: Stack -> Stack
add stack =
  if checkSize 2 stack
  then
    let (elems, newStack) = V.splitAt 2 (getStack stack)
        result = sum elems
     in push result (Stack newStack)
  else
    error "Stack underflow!"
```

Which brings us to our next helper: `checkSize`.

```Haskell
checkSize :: Int -> Stack -> Bool
checkSize requiredSize stack =
  (length $ getStack stack) >= requiredSize
```

Fundamentally, we need to be sure that the operation we make is safe at the
stack level. Size-indexed vectors do require a higher level of type-level programming
than the one that is required to read this series, and we will have to make do with
runtime checks.

Now that we have all the cards, let's combine them.
First, with the help of {{% tooltip "`V.splitAt`" "Int -> Vector a -> (Vector a, Vector a)" %}}, we grab a 2-tuple of vectors. The first one supposedly contains
the first two elements, and the second one has the rest of the stack in it.
With the help of `checkSize`, we then make sure to only proceed to the actual sum if *and only if* the first vector, `elems`, has two elements.
And finally, we push the result to the stack.

The subtraction function is similar in intent:

```Haskell
sub :: Stack -> Stack
sub stack =
  if checkSize 2 stack
  then
    let (elems, newStack) = V.splitAt 2 (getStack stack)
        result = sub' $ V.reverse elems
        sub' = foldl1 (-)
     in push result (Stack newStack)
  else
    error "Stack underflow!"
```

With the slight difference that we define our own subtraction function, and we reverse the vector beforehand so we get a correct result.

`foldl1` iterates over a container and applies the supplied function (`(-)`) over those elements while keeping an accumulator. By convention, the `1` suffix tells us that
we do not need to supply a initial accumulator to the recursive function, assuming a non-empty container to start with.  
You need to import that function from `Data.Foldable`:

```Haskell
import           Data.Foldable  (foldl1)
```

So far, we implemented addition and subtraction. Their definion were a bit convoluted,
unnessarily even, due to a lack of a better abstraction. But be patient.  
In [part 02](/post/write-a-forth-in-haskell-part-02), we will explore more traditional Forth operations, such as duplication, drop, and rotation, amongst others.
