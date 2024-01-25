+++
title = "Write a Forth in Haskell: Introduction"
date = 2020-01-04T20:51:31+01:00
description = "In which the author thinks that implementing a Forth is an acceptable pastime."
draft = false
toc = false
categories = ["Tutorials"]
tags = ["Forth", "Haskell", "PL"]
images = [
  "https://thedevs.network/static/img/posts/the-forth-programming-language.jpg"
] # overrides the site-wide open graph image
+++

This article is the first of a series on the implementation of the [Forth programming language](https://en.wikipedia.org/wiki/Forth_(programming_language)).

<!--more-->

* [Introduction](/post/write-a-forth-in-haskell-intro/) <--
* [Part 01](/post/write-a-forth-in-haskell-part-01/)
* [Part 02](/post/write-a-forth-in-haskell-part-02/)
* [Part 03](/post/write-a-forth-in-haskell-part-03/)

This series of article is about the implementation of a minimal Forth interpreter.

First invented in 1970, Forth has had a long life in several application domains: Space, embedded systems, and even in the FreeBSD bootloader.
Now, what makes Forth distinctive? Well, in opposition to more popular programming languages, Forth has an execution model that is deeply linked to its syntax.  

Forth operates on a stack, which makes it "stack-oriented".
Influenced by Lisp, its operator syntax uses what is called “reversed Polish notation” (RPN): The operator is marked **after** the operands.

An example Forth program is the following:

```Forth
1 2 +
```

And here you have it: The classic one-plus-two addition.
But Forth is not simply an RPN language, and you would be mistaken to think it only boils down to syntax.

Another example is:

```Forth
: addThousand 1000 + ; \ define the word `addThousand` as a function
                       \ that adds its operand and 1000.
10 addThousand         \ 1010
20                     \ push 20 on top the stack
+                      \ add 1010 and 20, giving us 1030
dup                    \ duplicate this value on the stack,
                       \ which now contains [1030, 1030]
over                   \ copy the second element from the top of the
                       \ stack to the top of the stack, giving us a
                       \ stack containing [1030, 1030, 1030]
```

This simplicity has the advantage of not needing parser combinators such as the Mighty [Megaparsec](https://hackage.haskell.org/package/megaparsec)
(or its venerable ancestor, [Parsec](https://hackage.haskell.org/package/parsec)).
This also has the advantage of allowing us to know the content of the stack when you encounter a binary (or dyadic) operator (An operator that has two arguments: addition, subtraction, etc).
With this knowledge, we can raise a "Stack Underflow" error if we already know that the stack's size is less than two, and we can stop consumming the input buffer.

This point of detail might seem useless, but Forth being used and run on extremely constrained hardware, it can greatly simplify the implementation code.

---

Throughout this tutorial, I will indicate the top of the stack with the convenient `<- Top` marker.  
I would also like to express my gratitude to [Nick Morgan](https://twitter.com/skilldrick) with his [Easyforth](https://skilldrick.github.io/easyforth/), as well as Richard W.M. Jones' own take on describing [the implementation of a Forth system](https://github.com/nornagon/jonesforth/blob/master/jonesforth.S).  
If some examples and wording in this series sound similar, it is because I can only copy the masters.

Additionally, I took advantage of the situation to learn myself how to use some less _naïve_ functional programming techniques, in an incremental way.

* In parts 01 and 02, we implement a state machine that processes the Forth instructions in a very simple way;
* In parts 03 and 04, the state machine will be refactored into using an effects system, adding IO capabilities to what started as a set of pure functions;
* Finally, part 05 will focus on adding new Forth constructs, and allowing the programmer to define their own *words* (variables or functions).

See you [in the next part](/post/write-a-forth-in-haskell-part-01), in which we will see how to implement some of the most common operations on the Forth stack.
