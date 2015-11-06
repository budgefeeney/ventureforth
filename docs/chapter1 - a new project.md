# Creating a Haskell Project

As Venture Forth will be a "proper" application it will have be comprised of the following parts

 * A library with all the core testable logic
 * An application using that library - typically this is just a single Main.hs project
 * A set of unit tests testing the library (only).
 	* We will also want to record both test coverage and documentation coverage.
 * A set of benchmarks evaluating funtion performance

Lets assume you're going to be working in `$HOME/Workspace/vforth`. Open a terminal and type the following

```
mkdir $HOME/Workspace/ventureforth
cd $HOME/Workspace/ventureforth
cabal init
```

This, interactively, creates a project for you. Most of the settings are self-explanatory. I'd recommend using a `src` directory for your code and a the `Haskell2010` dialect. 

We'll make a number of changes to this basic template. First we'll create some files:

```
touch README.md
mkdir app
touch app/Main.hs
touch src/Lib.hs
mkdir test
touch test/Spec.hs
mkdir bench
touch bench/Bench.hs
```

We'll populate these files later, but for the time being, we'll edit the `ventureforth.cabal` file. 

If you're an Atom user, you should at this page avoid it and use another text-editor instead, as Atom's Haskell support is reliant on having a fully formed Cabal file.

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
  exposed-modules:     Lib
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

Note that the app depends on a library called `ventureforth`. This is our library! The name comes explicitly from the `name` field at the top of the Cabal file. The added ghc options are

 * `-rtsopts` allows users to [configure the the Haskell runtime system (RTS) when launching your program](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html), for example to specify how many CPU cores to use for parallelism, or the maximum heap size. There are lists of RTS flags for [general usage](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html#rts-hooks), [parallelism](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-smp.html#parallel-options), [concurrency](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html) and [garbase collection](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html#rts-options-gc).
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
                       doctest >= 0.9 && < 0.11,
                       QuickCheck == 2.*,
                       HUnit -any,
                       test-framework -any,
                       test-framework-quickcheck2 -any,
                       test-framework-hunit -any  
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

This is broadly the same as for the executable, except that this is a `test-suite` section rather than an `executable` and we've specifed a test type: `exitcode-stdio`. This type of test is for test-apps which indicate failure using a non-zero executable exit code, and which print debug to stdout. The alternative is `detailed-0.9`, which is newer, cleaner, but less well supported. The [cabal manual](https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites) discusses this further.

Also, we're going to be using several libraries -- [doctest](https://hackage.haskell.org/package/doctest), [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.4.2) [(tutorial)](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing), [HUnit](https://hackage.haskell.org/package/HUnit) and [test-framework](https://hackage.haskell.org/package/test-framework) -- to create our test cases, so we include them here. This section is an example of Cabal's cleverness with dependent version numbers, using ranges and wildcards.

The final project is for benchmarking:

```
benchmark bench
    type: exitcode-stdio-1.0
    main-is: Bench.hs
    build-depends:
        base >= 4.7 && < 5,
        ventureforth -any,
        criterion == 1.*
    other-modules:
        Lib
    ghc-options: -Wall -Werror
```

This requires a benchmarking library called [criterion](https://hackage.haskell.org/package/criterion) [(tutorial)](http://www.serpentine.com/criterion/tutorial.html). All the modules (i.e. source-files) containing code to be benchmarked must appear in the `other-modules` section.
 
### Sandboxing

Before we start editing files, we'll need to make sure all our dependencies have been downloaded. While one could install them globally, this tends rapidly to lead to a situation where different applications and / or projects require different versions of a library, breaking other libraries in turn.

To avoid this "cabal hell", it's better for each project you undertake to have its own private library repository, called a [sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes). Create a sandbox in the project directory and then install all your project's required dependencies into it.

```
cabal sandbox init
cabal install --dependencies-only
```

Finally enter the following dummy code into `app/Main.hs` and `src/Lib.hs` respectively.

```
module Main where
import Lib

main :: IO ()
main = putStrLn welcomeMsg
```

```
module Lib (
  welcomeMsg
) where

-- | The text shown when the app launches
welcomeMsg :: String
welcomeMsg = "Wake up! It's time to venture forth."
```

and type `cabal build` to build it. The executable is

```
./dist/build/ventureforth/ventureforth
```

You can also create the documentation, using

```
cabal haddock
```

We are now ready to start editing our source files, and with a correct Cabal configuration can use Atom and IDE-Haskell to do so.

## A Module, a Test, and a Benchmark

For this first example, we'll create a small type to represent a location in our world, and make it showable. We'll add a test for the `show` function and a benchmark for the same function, as silly as that sounds. Out program will use this module to represent and display the first room's description to the user.

### The Module
All our types will be in a namespace called `VForth`, and we will re-export all of them from our `Lib` module.

In the `src` directory, create a subdirectory called `VForth` and within it create a module called `Location.hs` with the following contents.

```
module VForth.Location (
   Location(..)
 ) where

-- | A location in our game
data Location = Location {
    title :: String -- ^ The short title of the location
  , description :: String -- ^ A long description of the location
  }

instance Show Location where
  show l = title l ++ "\n"
         ++ replicate (length $ title l) '-' ++ "\n"
         ++ description l
```

This is very far from being the final `Location` type, we'll be revisiting throughout this tutorial

Next we ensure it's re-exported from our library by changing `Lib.hs` to the following. 

```
module Lib (
   module VForth.Location
 , welcomeMsg
 ) where

import VForth.Location

-- | The message shown when the application starts
welcomeMsg :: String
welcomeMsg = "Wake up! It's time to venture forth"
```

The changes are the addition of `module VForth.Location` in the module statement, and the required import of the re-exported module.

You can play around with this by starting the project in a REPL from the command using cabal. Here's an example of launching the REPL, creating a Location object, and printing it.

```
> cabal repl
Preprocessing library ventureforth-0.1.0.0...
GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling VForth.Location  ( src/VForth/Location.hs, interpreted )
[2 of 2] Compiling Lib              ( src/Lib.hs, interpreted )
Ok, modules loaded: Lib, VForth.Location.
*Lib> let l = Location { title="Hello", description="world" }
*Lib> putStrLn $ show l
Hello
-----
world
*Lib> 

```

Note that `Location` is only available because we re-exported it from our library. Likewise, for both testing and benchmarking, a necessary first step is the re-export.

### The Test

Now that we have a module, we'd like to create a few unit tests. All of the tests in this section are a bit silly, but are included as an illustrative example.

First we want to ensure that both titles and descriptions appear when we `show` a `Location`. We'll start with the most fragile approach first, a custom unit test.

Create a file `test/VForth/Locatoni


However a much more 

The `test` directory contains all the unit tests, again just to test this, type the following into `test/Spec.hs`:

```
module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = testDocCoverage

testDocCoverage :: IO()
testDocCoverage = doctest ["Lib.hs"]
```

This example uses the "doctest" library to create a unit-test which will fail if any function is not fully documented. Just call `doctest` with a list of the modules you want covered.

The `bench` directory contains all the  


This creates a folder called `vforth` and within it creates a project according to the "chrisdone" application template. You should edit the `vforth.cabal` file to fit your needs. It's mostly self-explanatory: the only subtlety is the difference between "synopsis" and "description".  In the template you're just referred to the `README.md` file, which you should also edit, as well as the licence file.

Next we create a cabal "sandbox". Since cabal tends to get confused when you try to install different versions of libraries ("cabal hell"), it's recommended to create project-specific repositories, to avoid any conflicts. Therefore, within your project directory, initialise the sandbox by typing:

```
cabal sandbox init
```