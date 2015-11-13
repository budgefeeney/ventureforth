# Venturing Forth

I've been teaching myself Haskell the last year, in order to learn a new way of thinking about program design, instead of just another syntax. There two most popular online resources, [Learn You a Haskell](http://learnyouahaskell.com/chapters) and the rather outdated [Real World Haskell](http://book.realworldhaskell.org/read/). Both do a good job of describing the language, but neither goes through the process of designing an functional applications, nor how to layout a "proper" Haskell project with unit-testing and code-coverage.

In the end, I found the best way of learning functional design was exploring F# tutorials, notably [this series](http://fsharpforfunandprofit.com/series/annotated-walkthroughs.html) on designing functional F# applications,  specifically [the "enterprise" tic-tac-toe example](http://fsharpforfunandprofit.com/posts/enterprise-tic-tac-toe/).

To similarly help other Haskellers, I have therefore decided to create a full, end-to-end application dealing with all the awkward aspects a real Haskell program must deal with: project layout, dependency management, unit-testing, I/O, configuration, and state. The target audience are people who have just finished reading [Learn You a Haskell](http://learnyouahaskell.com/chapters), but haven't yet written a full application.

To keep the focus on the core language, I've chosen to develop a [text adventure game](https://en.wikipedia.org/wiki/Interactive_fiction). While a pretty prehistoric class of app, but this is at least a little more fun than Fibonnacci series. The application will load the game world, load the user's saved state, then print a description of the room they're in and what items they're carrying. The user then types in commands like "move north" or "use wallet" or "use bag with clothes" to interact with that world. The particular game features a hungover student, an English town, and a bag of dirty laundry that needs cleaning.

This game is in fact a port of a game written as part of an Amstrad CPC BASIC tutorial published in issues [93](https://archive.org/details/amstrad-action-093), [94](https://archive.org/details/amstrad-action-094), [95](https://archive.org/details/amstrad-action-095) and [96](https://archive.org/details/amstrad-action-096) of [Amstrad Action](https://en.wikipedia.org/wiki/Amstrad_Action) magazine back in 1993.


In this first part of the tutorial we'll just

 1. Create a full project skeleton
 2. Use cabal, with a sandbox, to build it.
 
While simple, this is something not very well described elsewhere, and is somewhat tedious. However once done, this will act as a template for all future projects. Code for this part of the tutorial can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap1)

You'll need to have a fully functioning Haskell development environment setup with `cabal`, GHC 7.10 or later and ideally some sort of decent IDE. For instructions on how to set this up look at this [Haskell setup guide](fixme).



# Starting a new Haskell Project

> For educational purposes, in this and the next two parts of this tutoria we will project template very similar to the [Haskeleton]() template by hand. In practice you would just use the [Haskell Init scaffolding tool]() to create a project according to the Haskeleton template directly, as described [here](fixme) 

As a "proper" Haskell application, Venture Forth will be comprised of the following parts

 * A library with all the core testable logic
 * An application using that library - typically this is just a single Main.hs project
 * A set of unit tests testing the library (only).
 * A set of benchmarks evaluating performance of critical code-paths

Lets assume you're going to be working in `$HOME/Workspace/ventureforth`. Open a terminal and type the following

```
mkdir $HOME/Workspace/ventureforth
cd $HOME/Workspace/ventureforth
cabal init
```

This, interactively, creates a project for you. Most of the settings are self-explanatory. I'd recommend using a `src` directory for your code and a the `Haskell2010` dialect. 

We'll make a number of changes to this basic template. First, however, we'll create some files:

```
touch README.md
mkdir app
touch app/Main.hs
touch src/VForth.hs
mkdir test
touch test/Spec.hs
mkdir bench
touch bench/Bench.hs
```

Now we're ready to edit the `ventureforth.cabal` file. 

> If you're an Atom user, you should at this point avoid it and use another text-editor instead, as Atom's Haskell support is reliant on having a complete and well-formed Cabal file.

## Customising your Cabal File

### Metadata

In the initial metadata block, provide a description of your project. Values can span multiple lines if they are indented, so

```
description:
	Venture Forth is a text-based adventure game ported
	from the game of the same name developed in Amstrad
	Action, about a man, his laundry, and his need to
	clean them.
	
	It is a port of a game written as part of a Locomotive
	BASIC tutorial written in Amstrad Action magazine.
	
	This port is intended as a teaching example and so,
	like the original, is quite short.
```
Feel free to add more detail here.

If you're a GitHub user, you may find it useful to add a link to the bug-tracker:

```
bug-reports: http://github.com/username/ventureforth/issues
```

You'll also probably want to uncomment the copyright line and add the names of the owners.

Next add your README.md file as an "extra source file". If you're maintaining a changelog, add this here aswell.

```
extra-source-files:
    README.md
```

Finally, after the metadata, add information about source-control. This varies of course: GitHub users can enter

```
source-repository head
    type: git
    location: https://github.com/username/ventureforth
```

### Subprojects

With the metadata section filled, we can now create the four subprojects: for the library; for the app; for the test-suite; and for the benchmarks.

Add the following section for the library

```
library
  hs-source-dirs:      src
  exposed-modules:     VForth
  build-depends:       base >= 4.7 && < 5
  ghc-options:         -Wall -Werror
  default-language:    Haskell2010
```

The `ghc-options` parameter lists the flags passed to the compiler ([full list here](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flag-reference.html)) when compiling code. Haskell warnings are suitably valuable that I recommend turning al of them on (`-Wall`) and promoting them to errors (`-Werror`)

Next modify the section for the executable

```
executable ventureforth
  hs-source-dirs:      app
  main-is:             Main.hs
  build-depends:       base >= 4.7 && < 5,
                       ventureforth -any
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

Note that the app depends on a library called `ventureforth`. This is our library! The name comes explicitly from the `name` field at the top of the Cabal file. The additional ghc options are

 * `-rtsopts` allows users to [configure the the Haskell runtime system (RTS) when launching your program](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html), for example to specify how many CPU cores to use for parallelism, or the maximum heap size. There are lists of RTS flags for [general usage](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html#rts-hooks), [parallelism](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-smp.html#parallel-options), [concurrency](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html) and [garbage collection](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html#rts-options-gc).
 * `-threaded` ensures the the runtime must always support multithreaded programs - e.g. the garbage collector must be thread-safe
 * `-with-rtsopts` specifies the default RTS options. In this case `-N` sets the number of cores to use for automatic parallelism: with no number specified, it indicates that all available cores should be used.
 
Next add the section for the test project.

```
test-suite ventureforth-test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  build-depends:       base >= 4.7 && < 5,
                       ventureforth -any,
                       QuickCheck >= 2,
                       hspec >= 2.2
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

This is broadly the same as for the executable, except that this is a `test-suite` section rather than an `executable` and we've specifed a test type: `exitcode-stdio`. This type of test is for test-apps constructed as a command-line application, printing debug information to stdout, and returnin an exit code to the shell indicating if all tests passed or not. 

The alternative is `detailed-0.9`, which is newer, cleaner, but less well supported. The [cabal manual](https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites) discusses it further.

Also, we're going to be using several libraries to create our test cases, so we include them here. This section is an example of Cabal's cleverness with dependent version numbers: not only can one use ranges, but also wildcards as well. We'll discuss these different libraries when we come to actually writing unit-tests. Note that even though HSpec depends on QuickCheck, we need to explicitly include it in our Cabal file in order to use it ourselves, as we will do in [part three]()

The final sub-project is for benchmarking:

```
benchmark ventureforth-bench
    type: exitcode-stdio-1.0
    main-is: Bench.hs
    build-depends:
        base >= 4.7 && < 5,
        ventureforth -any,
        criterion == 1.*
    ghc-options: -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
```

This requires a benchmarking library called [criterion](https://hackage.haskell.org/package/criterion) [(tutorial)](http://www.serpentine.com/criterion/tutorial.html). 

Note that you can specify as many executables, test-suites and benchmarks as you want in a cabal file, provided they have different names. All will be built (and run if appropriate) by calling `cabal build`, `cabal test` and `cabal bench` on the command-line.
 
### Sandboxing

Now that we've amended our cabal file, we'll need to make sure all our dependencies have been downloaded. While one could install them globally, this tends rapidly to lead to a situation where different applications and / or projects require different versions of a library, breaking other libraries in turn.

To avoid this "cabal hell", it's better for each project you undertake to have its own private library repository, called a [sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes). Create a sandbox in the project directory and then install all your project's required dependencies into it.

```
cabal sandbox init
cabal install --only-dependencies
```

Note that Cabal automatically detects the presence of sandboxes, so we didn't have to tell cabal install to install into our sandbox.

Since we have other dependencies, for tests and benchmarking, we have to repeat this process

```
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```
```
cabal configure --enable-benchmark
cabal install --only-dependencies --enable-benchmark
```

Finally enter the following dummy code into `app/Main.hs` and `src/VForth.hs` respectively.

```
module Main where
import VForth

main :: IO ()
main = putStrLn welcomeMsg
```

```
module VForth (
  welcomeMsg
) where

-- | The text shown when the app launches
welcomeMsg :: String
welcomeMsg = "Wake up! It's time to venture forth."
```

## Testing it out

To compile just type `cabal build` to build it. The executable is

```
./dist/build/ventureforth/ventureforth
```

You can also create the documentation, using

```
cabal haddock
```

Finally, you can play with the code in your library using

```
cabal repl
```

In this case just the `welcomeMsg` function will be available.

We are now ready to start editing our source files, and with a correct Cabal configuration, we can at last use Atom and IDE-Haskell to do so. In [the next par of this tutorial] we'll add a module, a unit-test and a benchmark.


## Futher Reading:
Resources on how to create a Haskell project include [this example of starting a Haskell project](https://howistart.org/posts/haskell/1) and the [Holy Haskell Project Starter](http://yannesposito.com/Scratch/en/blog/Holy-Haskell-Starter/). 



-----
