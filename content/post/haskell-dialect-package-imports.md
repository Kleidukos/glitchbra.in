+++
title = "Package-qualified imports to reduce module hierarchy"
date = 2022-06-07T7:41:48+02:00
description = "Today in Hécate's Haskell Dialect: Package-qualified imports to reduce module hierarchy"
draft = true
[[copyright]]
  owner = "Hécate's blog"
  date = "2022"
  license = "cc-by-nd-4.0"
+++

[Package-qualified imports][pqi] are a GHC feature that allows you to specify the name of a package when importing a module. 
It looks like this:

```haskell
import "package-name" Module.Hierarchy (function)
```

The reader of an import statement can see the origin of an identifier easily. It is especially convenient if HLS does
not feel well and cannot tell you this when you need it.

At the same time in the ecosystem, we have a tradition of [long][dpsqq],
and [often][hdjco] [tedious][cmioc] module hierarchies.

In all these examples, we have a common hierarchy prefix, followed by the actual thing.

It is useful to easily determine the intended purpose of an identifier if it is
declared as part of this "family" of things that live under the same hierarchy.

However, I will argue that this has the disadvantage of having bloated import statements that take more
space on the screen to find the root of an identifier than to declare the identifier itself.
And I am guilty of doing this for some libraries that I publish as well, which is why I'm particuarly
sensitive to this issue.

There are also nuances to what I am saying here:

* In the case of [*transformers*][transformers], I find the package name clearer than
the `Control.Monad.Trans` hierarchy. We only reach the relevant part (`.Trans`) after
two levels deep in the hierarchy.

* However, some package names have a similar module hierarchy, which is descriptive enough that the 
module name does not immediately confer any advantage compared to the module hierarchy.
For example, the [cryptography-libsodium-bindings][clb] package has the following
hierarchy: `Cryptography.Sodium.Bindings`. 

In practice, this leads me to prefer

```haskell
import "transformers" Writer
```

over

```haskell
import Control.Monad.Trans.Writer
```

---

Now there is a problem: *transformers* doesn't actually export a `Writer` module, it's all under *Control.Monad.Trans*.

One immediate solution, as downstream consumers, is to use [cabal mixins][mixins] to rewrite the name of some modules specifically.
In practice, this would lead you to write:

```cabal
library
  mixins:
    transformers ( Control.Monad.Trans.Writer as Writer
                 , Control.Monad.Trans.Reader as Reader
                 )
```

You can then use this shortened version of the module name with the package name. 

This can be a good fit when you don't have that many modules and packages to which you will apply this technique.

As upstream producers for already-existing libraries, we can spread this practice in existing packages by:

1. Creating modules that are outside of this hierarchy that re-export the deeply nested modules.
The documentation of these modules will include a warning that they are to be used with
[`PackageImports`][pqi].
This has the inconvenient that Haddock [Named Chunks][named-chunks] are not re-exported, since
they are not identifiers.

2. Automatically generate a mixin config and provide it in the README.
This is a low-tech solution that I personally like, as the users of a library
can quickly copy-paste the configuration in their cabal file.

As for when you start a library, you have the opportunity to start from scratch with this pattern.

[pqi]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/package_qualified_imports.html
[dpsqq]: https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple-SqlQQ.html
[hdjco]: https://hackage.haskell.org/package/transformers-0.6.0.4/docs/Control-Monad-Trans-State-Strict.html
[cmioc]: https://hackage.haskell.org/package/base-4.16.1.0/docs/Control-Monad-IO-Class.html
[transformers]: https://hackage.haskell.org/package/transformers
[clb]: https://github.com/haskell-cryptography/cryptography-libsodium-bindings
[mixins]: https://cabal.readthedocs.io/en/3.6/cabal-package.html#pkg-field-mixins
[named-chunks]: https://haskell-haddock.readthedocs.io/en/latest/markup.html#named-chunks
