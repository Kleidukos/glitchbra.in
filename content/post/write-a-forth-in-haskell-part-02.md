+++
title = "Write a Forth in Haskell: Part 02"
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

* [Introduction](/post/write-a-forth-in-haskell-intro/)
* [Part 01](/post/write-a-forth-in-haskell-part-01/)
* [Part 02](/post/write-a-forth-in-haskell-part-02/) <--
* [Part 03](/post/write-a-forth-in-haskell-part-03/)

## The usual suspects

In this part, we will go beyond the simple numerical operations, and dive into stack manipulation. And while the title of this section refers to the 1995
movie, we are not going to deal with Keyser Söze *this* time.

### Dup

The first one is `dup`. Its "stack signature" can be expressed as `dup ( n -- n n )`, as it duplicates the top element of the stack.  
This means that given the following program 

```
1 2 3 dup <- Top
```

we will get the following stack as result:

```
1 2 3 3 <- Top
```

To add this capability to our interpreter, we need to do two things: The first one is to add this keyword to our `process` function's pattern-matching, and
the second one is to actually implement it. Let's get to it.


```Haskell
process :: Stack -> Text -> Stack
process stack "+"    = add stack
process stack "-"    = subtraction stack
process stack "dup"  = dup  stack

-- ...

dup :: Stack -> Stack
dup stack = push (V.head (getStack stack)) stack
```

As you can seen, this implementation of `dup` doesn't rely on a `pop` function. While this is certainly a questionable choice, I wanted to show what some
*naïve* implementation could look like.

Next in line are, as advertised…

### Drop, Swap, Over and Rot

`drop`'s stack signature is the following: `drop ( n -- )`. In itself, this does not tell us much
about its actual purpose: dropping the top element of the stack.

As an example, 

```Forth
1 2 3 drop <- Top
```

```Forth
1 2 <- Top

```

In the case of `swap`, the signature is `swap ( n1 n2 -- n2 n1 )`. In plain words, it means that
the top two elements of the stack are swapped.

For instance:

```Forth
1 2 3 4 swap <- Top
```

```Forth
1 2 4 3 <- Top
```

The next one is `over`. It duplicates the second topmost element, but puts the new element on the top of the stack. Its signature is `over ( n1 n2 -- n1 n2
n1 )`, and we can see it action here:

```Forth
1 2 3 over <- Top
```

```Forth
1 2 3 2 <- Top
```

And finally, `rot`. This one is slightly more messy. It pushes the third topmost element of the stack to the top, and by doing that pushes the other two
down.
Its stack signature is `rot ( n1 n2 n3 -- n2 n3 n1 )`, and it gives you:

```Forth
1 2 3 rot <- Top
```

```Forth
2 3 1 <- Top
```

### Implementing

Let's get to it.

`drop` is fairly simple to implement, as an operation on a vector.

```Haskell
drop :: Stack -> Stack
drop stack = Stack $ V.drop 1 (getStack stack)
```

We drop the first element of the stack, which returns a new stack.

`swap` is a bit more sophisticated. Since we do not have any kind of abstraction
on our stack, it looks like this:

```Haskell
swap :: Stack -> Stack
swap stack = Stack $ (V.reverse elems) <> newStack
  where
    (elems, newStack) = V.splitAt 2 (getStack stack)
```

We get the first two elements of the stack, by effectively splitting the stack at the second position.
From this operation, we get a `Vector` of size two, that we reverse before concatenating it with the `(<>)` operator.   
If you know some Ruby or Elixir, it
is akin to their, respectively, `(<<)` and `(<>)` operators for Strings, except that in Haskell any data structure can implement concatenation with `(<>)`. Even `Vector`.


`over`'s implementation is a bit more explicit.

```Haskell
over :: Stack -> Stack
over stack = Stack $ V.concat [e2, e1, e2, newStack]
  where
    (elems, newStack) = V.splitAt 2 (getStack stack)
    (e1, e2)          = V.splitAt 1 elems
```

By splitting the stack at the right places, we can reorder the elements in a final concatenation.

In the same vein, here is `rot`'s implementation.

```Haskell
rot :: Stack -> Stack
rot stack = Stack $ V.concat [newHead, newStack]
  where
    (elems, newStack) = V.splitAt 3 (getStack stack)
    [e1, e2, e3]      = V.toList elems
    newHead           = V.fromList [e3, e1, e2]
```

And finally, here is the final form of our `process` function:


```Haskell
process :: Stack -> Text -> Stack
process stack "+"    = add stack
process stack "-"    = subtraction stack
process stack "dup"  = dup  stack
process stack "drop" = drop stack
process stack "swap" = swap stack
process stack "over" = over stack
process stack "rot"  = rot  stack
process stack a      = push item stack
  where
    item = either (error . pack) fst (parseInt a)
```

And here we are! Let's meet in [part 03](/post/write-a-forth-in-haskell-part-03) to
make our interpreter evolve into something more useful and less clumsy regarding
stack operations.
