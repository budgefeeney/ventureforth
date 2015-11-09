# Installing a Haskell Coding Environment

Developing applications with Haskell requires several tools

 * GHC - the compiler
 * Cabal - which works as a dependency manager and a build system, a bit like Java's `maven`
 * An IDE[[1]](fixme). Several options exist, which we'll discuss below, but most rely on certain tools being present. These are:
 	* `ghc-mod` to provide support for syntax checking, linting, and autocompletion
	* `hlint` also provides standalone linting
	* `hoogle` allows you to search the documentation for a function or type
	* `stylish-haskell` is a code formatter
 
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

The [IDE Haskell plugin](https://atom.io/packages/ide-haskell) which works with the [Atom editor](http://atom.io) is the most promising at the moment, however while I've got it to work with GHC 7.10, it was after several false starts, and it frequently doesn't work.

The [IntelliJ IDEA](https://www.jetbrains.com/idea/) plugins, both IntelliJ's own and [HaskForce](https://carymrobbins.github.io/intellij-haskforce/) similarly struggle with GHC 7.10.

The [EclipseFP plugin](http://eclipsefp.github.io/) used to work very well indeed, but it relies on a tool called `buildwrapper` which ironically will not build with GHC 7.10 & Cabal 1.22

The [Leksah](http://leksah.org) application is a Haskell IDE written in Haskell which will definitely work. Visually it looks very poor on Mac OSX and Windows, and it has a slightly complicated layout that takes time to get used to.

Therefore the next best approach after Atom/IDE-Haskell, is an approach which is guaranteed to work: the web-based IDE provided by [FP Complete](http://www.fpcomplete.com). Obviously this requires a constant internet connection, and a github hosted project, but it does work well.

However it's worthwhile trying Atom first, so this is how to go about it.


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

You will likely need to configure each of these individually to enter the paths to cabal, ghc, ghc-mod etc. All applications should be found in `$HOME/.cabal/bin` with the exception of GHC. For Mac users it will be found in the GHC bundle, just type `which ghc` on the command-line to find the full path. Make sure that in the settings for haskell-ghc-mod you enter both the path `$HOME/.cabal/bin` to the "Additional Path Directories" field _and_ the path to the folder containing the ghc executable. These should be separated by a comma.

Then quit Atom, find a Haskell project (or create one as shown below) and restart Atom. 

Add the project directory using the "Add Project Folder" item of the "File" menu, then open its Cabal file to trigger the launch of the Haskell IDE support, complete with its eponymous "Haskell IDE" menu. If you're lucky, autocompletion, linting and building tools will all become available.

> Before opening a project with Atom, you may want to consider "test-driving" ghc-mod by having it test a file with `ghc-mod check /path/to/file.hs` and `ghc-mod link /path/to/file.hs`. It does occasionally do some very time-consuming things like build its own cabal or interact with stack, and this tends to confuse libraries mightily. Moreover, before you launch either Atom or `ghc-mod`, you should ensure that running `cabal build` in your project works before you open it for the __first__ time in Atom (it's not a requirement for subsequent launches)



## The IHaskell REPL
A good REPL is invaluable when learning a new language, a new library, or just playing around with ideas. IHaskell is the best Haskel REPL I've come across.

It's based on [IPython Notebook](http://ipython.org/notebook.html), a web-frontend that lets you interleave markdown and live code in a virtual notebook. Over the summer this was split into two projects, a ["Jupyter"](http://jupyter.org/) core and a Python plugin. This was to explicitly accommodate other languages, such as Haskell, using the [IHaskell](https://github.com/gibiansky/IHaskell) plugin. 

Linux users can install jupyter using their package manager, though depending on their distribution it may still be called ipython, and then install ihaskell using

```
cabal install ihaskell
ihaskell install
```

Once installed, launch the notebook server by typing `ipython notebook`. Despite the name, you will see you will have the option of creating either Python or Haskell notebooks.

For OS X users you can use Homebrew to install Python, Pip to install ipython, and then cabal to install IHaskell as shown above. Should you try to install Python and IPython using MacPorts, you immediately encounter a linking issue, as the relocated GHC bundle we chose above links to OS X's system libraries, while MacPort's Python links to its own conflicting versions of the same libraries. The easiest, though spectacularly wasteful, solution to this is to install [Kronos Haskell](http://www.kronosnotebook.com/haskell) which is a 2GB app bundle with its own copies of Python, Jupyter, GHC, Cabal and the usual Haskell libraries.

For windows users, the Kronos option is by far and away the simplest approach.

# Creating your first project

For this, we use `stack`. Assume you want to create a project called "myproject" in `$HOME/Workspace/myproject`

The steps are

```
cd $HOME/Workspace
stack new myproject chrisdone
```
This creates a new project, called `myproject` according to the "chrisdone" project template for standalone apps. This creates the cabal file, a README.md and a licence file, and three source directories for the three subprojects:
 
 * A library with all the testable application logic, implicitly called "myproject", in `src`
 * An application with all the untestable logic, explicitly called "myproject-exe" which depends on the "myproject" library, in `app`. Often this will just have a single `Main.hs` script to launch the application.
 * A test-suite, explicitly called "myproject-test", which obviously depends on the "myproject" library it tests, in `tests`.
 
You should edit the `.cabal` file to change to suit your needs. Most fields are pretty obvious, though the difference between "synopsis" and "description" is rather subtle: the former is a short single-sentence description of your project, the latter is a paragraph or two describing it in depth.

As Stack is not fully supported by many IDEs, it's probably a good idea to delete the stack project files that exist in parallel to cabal. These are:

 * FIXME
 * and FIXME



# Actually Learning Haskell

There are two free online resources that are popular:

* [Learn You a Haskell (LYAH)](http://learnyouahaskell.com) is very slightly out of date[[3]](), is extremely readable, but ends without ever demonstrating how to code, let alone design, a full application
* [Real World Haskell](http://book.realworldhaskell.org) is _horribly_ out of date, and tends to work through doing things the hard way before showing how they can be done simply - an approach which makes learning Haskell unnecessarily tedious. It does, however, delve into much more of the practicalities of writing real-world applications, and the commenters have provided corrections in the cases where the book has fallen out of date. It's best read after LYAH to fill in a few gaps.

The best resource is actually the print book [Beginning Haskell: A Project-Based Approach](http://www.amazon.com/Beginning-Haskell-A-Project-Based-Approach/dp/1430262508), and while obviously more expensive than free, I do think it pays its own way in terms of time saved.


---

 1. Many students, academics and indies will often play down the need for IDEs, claiming that proficient developers need only use an editor. And for single-person projects, they may be right. However when working on a major project with hundreds of files developed over several years, by several dozen people who have come and gone, the tools IDEs provide for autocompletion, inline doc-viewing, linting, and debugging are invaluable.
 
 2. It has to be said, Haskell has suffered a little from an overabundance of enthusiastic amateurs and a corresponding lack of dull professionals. There are many libraries and dev-tools with new versions being released continuously: fewer people are writing apps, and fewer still are working in commerical teams writing apps. The ecosystem reflects this: there are few languages where setting up a stable development environment is this difficult - at least on a Mac.
 
 3. Specically the eponymous constructors for the `State`, `Writer` and `Reader` monads have all been hidden, and instead replaced with lower-case functions `state`, `writer` and `reader`, affecting the code examples in chapter 13.