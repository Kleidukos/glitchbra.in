+++
title = "Stepping down from cabal release coordination"
date = 2024-03-21T10:10:02+01:00
description = "Wrapping up my tenure as release coordinator for cabal"
draft = false
toc = false
categories = ["announcements"]
tags = ["cabal", "haskell"]
+++

I had a fantastic time as a release coordinator for the [Cabal project][cabal], and it is
with emotion that I am stepping down from this position.

## How I arrived there

I had the privilege of arriving in orbit of the Cabal development team when it was in the middle
of a cultural shift. Newcomers were actively welcomed and introduced to the project, encouraged
to leave reviews despite not being maintainers, to acquaint themselves with the processes and
codebases.

I was given trust, and the opportunity to learn. I fucked up some times, but always got back up.
After all, even monkeys fall from the trees.

## What I leave behind

I am proud to say that I have set-up some procedures to help with the structure of a development 
community that can welcome newcomers from more fronts now.

Since the establishment of the [Quality Assurance programme][QA], newcomers can start contributing
to the project in meaningful ways without having to fully understand the inner workings of all 
the components making up `cabal-install`. By having a focus on user experience and how users 
perceive the interactions with the tool, we are better able to keep `cabal-install` in line
with the expectations of our user base as it evolves.
This also allowed us to ask for [more specific help][QA-windows] testing `cabal-install` for platforms that
have historically been underprivileged in the Haskell ecosystem.

Being a professional engineer, I also had the chance of being able to invite a colleague, who is a
QA engineer, so that she could educate us. Her initial feedback was very useful to me, as I
was not feeling like re-inventing the whole practice of Quality Assurance from scratch for the purpose of Cabal.

This has also boosted the usage of Nightly releases of `cabal-install`, and I am glad that we are
able to distribute them smoothly with the collaboration of [ghcup][ghcup].

The transparency of our meetings is also important. I took the habit of transcribing our
fortnightly calls. Then started to optimise the time unpaid volunteers were spending together,
by loosely modelling the meeting agenda against the
[Eisenhower decision matrix](https://luxafor.com/wp-content/uploads/2023/02/The-Eisenhower-Decision-Matrix-png-1024x768.png).
Actively bringing people who were only briefly involved with us was also a move of which I am proud,
as I believe it contributed to demystify the decision-making process.

## What is next

I am bringing all this positive energy for change and collaboration to the [Haddock project][haddock]!
Being a notorious idiot, I need to read documentation, and thus documentation needs to be written under the best conditions. 
This is why I am devoting time and energy improving the process of authoring for Haskell projects.
This is all work that I am doing as part of my continued involvement in the [Haskell Foundation][HF]'s projects.
Not sitting at the Foundation's Board does not mean I cannot bring positive change in our ecosystem. 

I am not leaving the Cabal project for good, as there will be a transition period where I'm handing off my knowledge and practices.
But I believe it is time for someone else to have the immense privilege of being involved in coordinating releases of cabal.

## <3

I am very proud of all the people who spent time making cabal better. Thank you all, your support was essential and I could not have
done half of what I did without it.

[cabal]: https://www.haskell.org/cabal
[QA]: https://discourse.haskell.org/t/manual-qa-for-cabal-install/6225
[QA-windows]: https://discourse.haskell.org/t/cabal-is-looking-for-qa-testers-on-the-windows-platform/8103
[ghcup]: https://github.com/haskell/cabal#ways-to-get-the-cabal-install-binary
[haddock]: https://www.haskell.org/haddock
[HF]: https://haskell.foundation
