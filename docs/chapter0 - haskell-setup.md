# Installing a Haskell Coding Environment

Developing applications with Haskell requires several tools

 * GHC - the compiler
 * Cabal - which works as a dependency manager and a build system, a bit like Java's `maven`
 * An IDE[[1]](fixme). Several options exist, which we'll discuss below, but most rely on certain tools being present. These are:
 	* `ghc-mod` to provide support for syntax checking, linting, and autocompletion
	* `hlint` also provides standalone linting
	* `hoogle` allows you to search the documentation for a function or type
	* `stylish-haskell` is a code formatter
 * A project scaffolding tool called `hi` (Haskell Init)
 
It is, unfortunately, embarrassingly difficult to get all three installed and working together, particularly if you're not using Linux. The main problem is that many verstions of the compiler, tools and libraries exist, and installing the versions that happen to work well together is tricky[[2]](fixme).
 
In recent times, the [Stackage](https://www.stackage.org) project has been launched by the people at [FP Complete](http://www.fpcomplete.com) to address this. Stackage maintains a collection of particular Haskell libraries and applications, at specific versions, which are all tested to not only be stable themselves, but stable with respect to each other. It is similar to a Linux distribution.

However we will be largely eschewing stack for the time being, as none of the available Haskell IDEs currently work with it.

# Setting Up Haskell

The minimum version to target for GHC is 7.10, and for Cabal, it's 1.22.


For Mac OSX, the easiest way is the [relocatable GHC package](https://ghcformacosx.github.io/) which comes with GHC, cabal and stack. Note that you will already need to have installed XCode, launched it, and then used it to install the  "Commandline tools" (via the "Downloads" tab in its preferences).

For Windows users, the [Haskell Platform](https://www.haskell.org/platform/) is the best option. Once installed, install the `stack` tool directly from [its website](https://github.com/commercialhaskell/stack/blob/master/doc/install_and_upgrade.md) 

For Linux users, the first option is obviously their native package manager, but if that doesn't have GHC 7.10, then the Haskell Platform and separate stack install is the next best option.

Once done, you should have `ghc`, `ghci`, `runghc`, `stack` and `cabal` all on your path. GHC is the compiler obviously, `ghci` is a simple REPL, and `runghc` compiles and runs the given script.

The next step is just to update the Cabal package database to the latest version, using

```
cabal update
cabal install cabal cabal-install
```

For those who are confused, `cabal` is the library and `cabal-install` is the application called -- somewhat confusingly -- `cabal`.




# Installing an IDE

The simple truth is that as of November 2015, I have found no process which is guaranteed to provide a new user with an IDE that works reliably, at least not on a Mac.

The [IDE Haskell plugin](https://atom.io/packages/ide-haskell) which works with the [Atom editor](http://atom.io) is the most promising at the moment, however while I've got it to work with GHC 7.10, it was after several false starts. Ghc-mod, on which it relies, will often crash out silently due to misformed Haskell or Cabal files, in which case you need to relaunch the editor. At other times, ghc-mod will launch some expensive, long-running process that confuses Atom. 

The [IntelliJ IDEA](https://www.jetbrains.com/idea/) plugins, both IntelliJ's own and [HaskForce](https://carymrobbins.github.io/intellij-haskforce/) similarly struggle with GHC 7.10 and ghc-mod.

The [EclipseFP plugin](http://eclipsefp.github.io/) used to work very well indeed, but it relies on a tool called `buildwrapper` which ironically will not build with GHC 7.10 & Cabal 1.22

The [Leksah](http://leksah.org) application is a Haskell IDE written in Haskell which will definitely work. Visually it looks very poor on Mac OSX and Windows, and it has a slightly complicated and unorthodox layout that takes time to get used to.

Therefore the next best approach after Atom/IDE-Haskell, is an approach which is guaranteed to work: the web-based IDE provided by [FP Complete](http://www.fpcomplete.com). Obviously this requires a constant internet connection, and a github hosted project, but it does work well.

Since Atom, when it works, works very well indeed, it's worthwhile trying to install and configure it however, which is what we describe next.


## Atom IDE-Haskell Installation

Install [Atom](http://atom.io) and then install the Haskell IDE support tools:

```
cabal install happy
cabal install ghc-mod hlint hoogle stylish-haskell
```

Happy needs to be installed first and separately as it is an undeclared dependency of `haskell-src-exts`, which is in turn required by `ghc-mod`.

These applications are installed to `$HOME/.cabal/bin` so make sure it's on your path.

Then launch Atom and install the following packages:

 * haskell-ghc-mod
 * autocomplete-haskell 
 * language-haskell 
 * ide-haskell

You will likely need to configure each of these individually to enter the paths to cabal, ghc, ghc-mod etc. All applications should be found in `$HOME/.cabal/bin` with the exception of GHC. For Mac users it will be found in the GHC bundle, just type `which ghc` on the command-line to find the full path. Make sure that in the settings for haskell-ghc-mod you enter both the path `$HOME/.cabal/bin` to the "Additional Path Directories" field _and_ the path to the folder containing the ghc executable. These should be separated by a comma. Lastly, don't actually enter `$HOME` in these configuration dialogs, enter the full path to your home directory.

Then quit Atom, find a Haskell project (or create one as shown below) and restart Atom. 

Add the project directory using the "Add Project Folder" item of the "File" menu, then open its Cabal file to trigger the launch of the Haskell IDE support, complete with its eponymous "Haskell IDE" menu. If you're lucky, autocompletion, error-detection, linting and building tools will all become available.

> Before opening a project with Atom, you may want to consider "test-driving" ghc-mod by having it test a file with `ghc-mod check /path/to/file.hs` and `ghc-mod lint /path/to/file.hs`. It does occasionally do some very time-consuming things like build its own cabal or interact with stack, and this tends to confuse IDEs mightily. Moreover, before you launch either Atom or `ghc-mod` for the first time on a new project, you should ensure that running `cabal build` works first.



## The IHaskell REPL
A good REPL is invaluable when learning a new language, a new library, or just playing around with ideas. IHaskell is the best Haskel REPL I've come across.

It's based on [IPython Notebook](http://ipython.org/notebook.html), a web-frontend that lets you interleave markdown and live code in a virtual notebook. Over the summer this was split into two projects, a ["Jupyter"](http://jupyter.org/) core and a Python plugin. This was to explicitly accommodate other languages, such as Haskell, using the [IHaskell](https://github.com/gibiansky/IHaskell) plugin. 

Linux users can install jupyter using their package manager, though depending on their distribution it may still be called ipython, and then install ihaskell using

```
cabal install ihaskell
ihaskell install
```

Once installed, launch the notebook server by typing `ipython notebook`. Despite the name, you will see you will have the option of creating either Python or Haskell notebooks.

For OS X users you can use [Homebrew to install Python, Pip to install ipython](https://joernhees.de/blog/2014/02/25/scientific-python-on-mac-os-x-10-9-with-homebrew/), and then cabal to install IHaskell as shown above. Should you try to install Python and IPython using MacPorts, you immediately encounter a linking issue, as the relocated GHC bundle we chose above links to OS X's system libraries, while MacPort's Python links to its own conflicting versions of those same libraries. The easiest, though spectacularly wasteful, solution to this is to install [Kronos Haskell](http://www.kronosnotebook.com/haskell) which is a 2GB app bundle with its own copies of Python, Jupyter, GHC, Cabal and the usual Haskell libraries.

For windows users, the Kronos option is by far and away the simplest approach.

# Creating your first project

We'll use the [Haskell Init tool](https://github.com/fujimura/hi). If you haven't already installed it, type 

```
cabal install hi
```

and then to create a project called `my-project-name` in a directory of the same name type

```
hi my-project-name \
	--repository git://github.com/tfausak/haskeleton.git \
	--package-name my-project-name \
	--module-name MyProjectName \
	--author Bryan Feeney \
	--email bryan.feeney@mailserver.com
```

The repository flag gives a path to a template project. The Haskeleton template features unit-tests, documentation tests and benchmarks, with all of these explained in the [Haskeleton guide](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/). Other project tempaltes, such as web-apps, can be found on the [HI templates page](https://github.com/fujimura/hi/wiki#available-templates))

Module names are used in Haskell code, package names are used on the [Hackage package repository](http://hackage.haskell.org). The module-name is usually just a camel-cased valid-Haskell version of the typically dashed package name.

Before you start coding, you'll want to download the skeleton project's dependencies. Rather than mix these dependent packages in with your global package repository, create a local package sandbox for your project:

```
cd myprojectname
cabal init sandbox

cabal install --only-dependencies
cabal install --enable-test  --only-dependencies
cabal install --enable-bench --only-dependencies
```

Cabal can automatically detect a sandbox, so once it's created you can just use the usual commands. Typing `cabal install --only-dependencies` installs the project's dependencies as given in the Cabal file. By default this is for the library and/or executable only. You need additional calls to download and install any dependencies required for the unit-tests and benchmarks.

As a quick check, make sure everything builds:

```
cabal build
```

And then you're safe to open the project in Atom. Obviously you can also run tests using `cabal test`, and execute the benchmarks using `cabal bench`, though if you want benchmarks presented as graphs rather than ASCII text, type:

```
mkdir -p dist/bench
cabal bench --benchmark-options="--output dist/bench/index.html"
```

Note benchmarks are only available with the Haskeleton project template.


# Actually Learning Haskell

There are two free online resources that are popular:

* [Learn You a Haskell (LYAH)](http://learnyouahaskell.com) is very slightly out of date[[3]](), is extremely readable, but ends without ever demonstrating how to code, let alone design, a full application
* [Real World Haskell](http://book.realworldhaskell.org) is _horribly_ out of date, and tends to work through doing things the hard way before showing how they can be done simply - an approach which makes learning Haskell unnecessarily tedious. It does, however, delve into much more of the practicalities of writing real-world applications, and the commenters have provided corrections in the cases where the book has fallen out of date. It's best read after LYAH to fill in a few gaps.

The best resource however is actually the print book [Beginning Haskell: A Project-Based Approach](http://www.amazon.com/Beginning-Haskell-A-Project-Based-Approach/dp/1430262508), and while obviously more expensive than free, I do think it pays its own way in terms of time saved.


---

 1. Many students, academics and indies will often play down the need for IDEs, claiming that proficient developers need only use an editor. And for single-person projects, they may be right. However when working on a major project with hundreds of files developed over several years, by several dozen people who have come and gone, the tools IDEs provide for autocompletion, inline doc-viewing, linting, and debugging are invaluable.
 
 2. It has to be said, Haskell has suffered a little from an overabundance of enthusiastic amateurs. New versions of libraries and dev-tools with new versions being released continuously, with breaking changes. It'a rather unstable eco-system, with few people working in teams on large open-source apps. The upshot is that there are few languages where setting up a stable development environment is this difficult - at least on a Mac.
 
 3. Specifically the eponymous constructors for the `State`, `Writer` and `Reader` monads have all been hidden, and instead replaced with lower-case functions `state`, `writer` and `reader`, affecting the code examples in chapter 13.