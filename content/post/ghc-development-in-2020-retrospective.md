+++
title = "GHC development in 2020: My retrospective"
date = 2021-02-04T09:46:10+02:00
description = "Wrapping up what was done on the documentation front"
categories = ["Activity Report"]
tags = ["Haskell", "Community"]
draft = false
+++

This article is written for the release of [GHC 9.0.1][GHC 9.0.1]. While this is
an important release, it also marks a year, or so, since I have started to
contribute to the GHC project.

## The Haskell Docs Initiative genesis

Ten months ago, on March 17th at 9:52AM, I opened my first ticket on the GHC issue tracker,
asking for the inclusion of doctests in the `base` library.
At 10:23 the same day, I would open the [Documenting Base][Documenting Base]
tracking issue, that would be the face of the Haskell Docs initiative. 

### Gathering contributors

Let us be honest, "fixing" `base`'s documentation is not something one contributor
could (or even should want to) do alone. Fortunately, we have several tools enabling
us to gather community support for such tasks, and by leveraging the power of social
media, the initiative managed to attract several first-time contributors to help
us round the edges of the documentation:

{{< tweet 1243142712263692288 >}}

Now, how does one successfully bootstraps a documentation contributor?
They do not need the validation test suites to pass, nor do they need a variety
of platforms against which to test their changes.

You need to explain how to set up a working environment,
show the commands that they will need, and establish a formal protocol 
that they can simply follow, and thus free their mind for the task at hand: improving the docs.  
The process was refined multiple times (it would have been idyllic to think
that it would have stayed the same from the beginning), and fortunately I was
able to piggy-back on the fantastic work done by the folks at Serokell, with
the [ghc.dev][ghc.dev] web page, that itself explains well how to set up a
working GHC environment for contributors.


### Protocol, etiquette, guidelines

As mentioned above, the best way to ensure contributions is to streamline the
process so that the contributors can follow a list of easy steps to integrate
their work in the project.  
Thus, A “merge request etiquette” was written, so that the contributions would be more
easily integrated into the issue tracker, and more easily reviewable   
(Haddock can be tricky and I would be lying if I said that I have never been bitten by it).

One of the things that first came to mind was that there would inevitably be people
from outside of the GHC development community, and they would work with long-time contributors.
Those new contributors would have to learn things of technical nature
but also about the social interactions in place,
and getting properly included in a pre-existing community that has not already
dozens of regular users can be hard! Moreover, knowing full well that merge requests can go unnoticed for months,
[Carter Schonwald][Carter] and I made ourselves available for these contributions.

That is why alongside the technical details, I took the time to lay down in plain
words that we would be operating in the context of the GHC project, that their
[guidelines for respectful communication][guidelines] still applied, and that
the co-chairs of the GHC Steering Committee were available should any incident
occur. 
The idea was that the tracking issue would be the façade of the project, and should
be unequivocal about the values of our community.

### <3

I am incredibly proud of all the people who were involved. I truly believe we
opened the doors to more contributors, and it is something that I still
put forward when people ask me how they can contribute to the project.


## Dusting off the code-base: Dancing With The Moonlint Knight

Maybe you don't already know it, but GHC is actually older than me. Its first
release was in 1992, and software is not French wine! Three months after starting
the calls for contributors, I undertook another kind of project: Dusting off the
code-base. Actually, code-base**s**. The GHC tree is divided in several sub-projects,
including the RTS, the compiler and the base library.

In July of 2020, I started a discussion on the [integration of HLint][HLint discussion]
in our continuous integration system. We were extremely lucky to have Neil Mitchell and
Shayne Fletcher involved in it, the former giving us solid advice and the latter
being available day and night when we hit a roadblock with HLint. I am immensely 
grateful to them.

Since then, HLint has been more and more integrated in the CI process,
and I took more responsibilities with respect to the CI infrastructure, since I
had to make it evolve to enable us to have source code linting without adding
excessive latencies.

## Moving forward

I wanted to write this article for the release of GHC 9.0.1, but it also coincides
with another major event in the Haskell community: The creation of the [Haskell Foundation][HF],
and my nomination to its Board. This is certainly one of the most important events
of my Haskell life, and I have great plans for our community. :)  
The main challenge is going to keep our momentum going, fund some decisive parts
of both GHC development and community infrastructure.

The main thing on which I am focussed is the vertical integration of our documentation
infrastructure, from the documentation style guide to the Haddock tool, and the GHCi [:doc][:doc]
command to browse the docs from the REPL.

I wish to express my deepest gratitude to the people who enabled my crazy antics
and believed in this sacred duty that is to take care of our old ones.

I hope it will be my privilege in the future to enable younger contributors 
to keep Haskell such a joy.


[GHC 9.0.1]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/9.0.1-notes.html
[Documenting Base]: https://gitlab.haskell.org/ghc/ghc/-/issues/17929
[ghc.dev]: https://ghc.dev/
[guidelines]: https://github.com/ghc-proposals/ghc-proposals/blob/master/GRC.rst
[HLint discussion]: https://gitlab.haskell.org/ghc/ghc/-/issues/18424
[Carter]: https://twitter.com/cartazio
[HF]: https://haskell.foundation/
[:doc]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-:doc
