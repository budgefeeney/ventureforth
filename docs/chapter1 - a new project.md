# Creating a Haskell Project

**FIXME** Use name VForth.hs instead of Lib.sh to avoid name clashes if application used as a library

As Venture Forth will be a "proper" application it will have be comprised of the following parts

 * A library with all the core testable logic
 * An application using that library - typically this is just a single Main.hs project
 * A set of unit tests testing the library (only).
 	* We will also want to record both test coverage and documentation coverage.
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
touch src/Lib.hs
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
                       doctest >= 0.9 && < 0.11,
                       hspec >= 2.2
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

This is broadly the same as for the executable, except that this is a `test-suite` section rather than an `executable` and we've specifed a test type: `exitcode-stdio`. This type of test is for test-apps which indicate failure using a non-zero executable exit code, and which print debug to stdout. The alternative is `detailed-0.9`, which is newer, cleaner, but less well supported. The [cabal manual](https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites) discusses this further.

Also, we're going to be using several libraries to create our test cases, so we include them here. This section is an example of Cabal's cleverness with dependent version numbers, using ranges and wildcards. We'll discuss these different libraries when we come to actually writing unit-tests.

One thing to note is that HSpec automatically brings in the QuickCheck and HUnit libraries, which we'll discuss later.

The final sub-project is for benchmarking:

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
    ghc-options: -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
```

This requires a benchmarking library called [criterion](https://hackage.haskell.org/package/criterion) [(tutorial)](http://www.serpentine.com/criterion/tutorial.html). All the modules (i.e. source-files) containing code to be benchmarked must appear in the `other-modules` section. In our case, we just use the top-level library module.
 
### Sandboxing

Now that we've amended our cabal file, we'll need to make sure all our dependencies have been downloaded. While one could install them globally, this tends rapidly to lead to a situation where different applications and / or projects require different versions of a library, breaking other libraries in turn.

To avoid this "cabal hell", it's better for each project you undertake to have its own private library repository, called a [sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#developing-with-sandboxes). Create a sandbox in the project directory and then install all your project's required dependencies into it.

```
cabal sandbox init
cabal install --only-dependencies
```

Since we have other dependencies, for tests and benchmarking, we have to repeat this process

```
cabal configure --enable-tests
cabal install --only-dependencies --enable-tests
```
```
cabal configure --enable-benchmark
cabal install --only-dependencies --enable-benchmark
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

We are now ready to start editing our source files, and with a correct Cabal configuration, we can at last use Atom and IDE-Haskell to do so.

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

Now that we have a module, we'd like to create a few unit tests. There are two popular test libraries

 * [HUnit](https://hackage.haskell.org/package/HUnit) - is the classic Java style unit-test library, where functions like `assertEquals` are used to ensure the result you got matches the result you expected
 * [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.4.2) [(tutorial)](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) is a slightly more powerful library, where you specify a property that should hold, and then quick-check will generate many sample inputs at random and see if the property is upheld.
 
On top of that [doctest](https://hackage.haskell.org/package/doctest) is a library that creates an automatic unit test which verifies all functions and properties have documentation.

As developers will often use _both_ HUnit and QuickCheck, libraries have been created to abstract over them. The two most common of these abstraction libraries are [test-framework](https://hackage.haskell.org/package/test-framework) and [HSpec](http://hspec.github.io/). In this application I'll be using HSpec.


Our first test is a classic unit test, to ensure that both titles and descriptions appear when we `show` a `Location`. HSpec abstracts HUnit's syntax, so we have functions like `shouldBe` in place of `assertEquals`. HSpec also requires that all test-modules' names end with `Spec`

So create a file `test/VForth/LocationSpec.hs` with the following contents

```
module VForth.LocationSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Location Display" $ do
    it "Show puts title before dashes before a description" $ do
      let
        l = Location { title="My Title", description="The complete description." }
      in
      show l `shouldBe` "My Title\n--------\nThe complete description."

    it "Title should be included in description" $ do
      let
        l = Location { title="My Title", description="The complete description." }
      in
      show l `shouldContain` "My Title"

    it "Title should be included in description" $ do
      let
        l = Location { title="My Title", description="The complete description." }
      in
      show l `shouldContain` "The complete description."
```

The indentation is crucial for reading and compiling this. Use the "description" block to identify a test group, and the "it" commands to say what each test is doing. The full list of "shouldXXX" functions are listed on [HSpec's Hackage page](https://hackage.haskell.org/package/hspec-expectations-0.7.2/docs/Test-Hspec-Expectations.html).




However a much more 

The `test` directory contains all the unit tests, again just to test this, type the following into `test/Spec.hs`:

```
import Test.Hspec
import qualified VForth.LocationSpec as LocationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Location" LocationSpec.spec
```


There are two things to note here. The first is the absence of a `module` header at the top of the file: this is necessary to run the test through Cabal, which is silly, yet true.

The second is that you can have several tests listed in the `spec` function, e.g.

```
spec :: Spec
spec = do
	describe "Location" LocationSpec.spec
	describe "Item"     ItemSpec.spec
	describe "Player"   PlayerSpec.spec
```

To run this test, type

```
cabal test
```

on the command-line. 

> `ghc-mod` appears not to handle test-dependencies particularly well: you may find it, and therefore Atom, complain about a missing Test.HSpec pacakge until you execute `cabal test` on the command-line


This example uses the "doctest" library to create a unit-test which will fail if any function is not fully documented. Just call `doctest` with a list of the modules you want covered.

The `bench` directory contains all the  


This creates a folder called `vforth` and within it creates a project according to the "chrisdone" application template. You should edit the `vforth.cabal` file to fit your needs. It's mostly self-explanatory: the only subtlety is the difference between "synopsis" and "description".  In the template you're just referred to the `README.md` file, which you should also edit, as well as the licence file.

Next we create a cabal "sandbox". Since cabal tends to get confused when you try to install different versions of libraries ("cabal hell"), it's recommended to create project-specific repositories, to avoid any conflicts. Therefore, within your project directory, initialise the sandbox by typing:

```
cabal sandbox init
```