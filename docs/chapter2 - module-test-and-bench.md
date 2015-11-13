# Venture Forth: A Module, a Test, and a Benchmark

This is the second part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part was [building a skeleton project](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap2)

In this part we'll

1. Create a simple module
2. Experiment with it using `cabal repl`
3. Test that module with a simple a unit-test using [HSpec](http://hspec.github.io)
4. Evaluate that module's performance using [Criterion](http://www.serpentine.com/criterion/tutorial.html)


## The Module
All our types will be in a namespace called `VForth`, and we will re-export all of them from our `VForth` module.

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

As you can see we've used Haddock ([tutorial](https://www.haskell.org/haddock/doc/html/markup.html#idm140354810917952)) for our documentation. We'll be revising this module continuously throughout as we progress.

Next we ensure it's re-exported from our library by changing `VForth.hs` to the following. 

```
module VForth (
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
[2 of 2] Compiling VForth           ( src/VForth, interpreted )
Ok, modules loaded: VForth, VForth.Location.
*Lib> let l = Location { title="Hello", description="world" }
*Lib> putStrLn $ show l
Hello
-----
world
*Lib> 

```

Note that `Location` is only available because we re-exported it from our library.

## The Test

Now that we have a module, we'd like to create a few unit tests. There are two popular test libraries

 * [HUnit](https://hackage.haskell.org/package/HUnit) - is the classic Java style unit-test library, where functions like `assertEquals` are used to ensure the result you got matches the result you expected
 * [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.4.2) [(tutorial)](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing) is a slightly more powerful library, where you specify a property that should hold, and then quick-check will generate many sample inputs at random and see if the property is upheld.
 
Additionally [doctest](https://hackage.haskell.org/package/doctest) verifies that all code-samples in comments are correct.

As developers will often use _both_ HUnit and QuickCheck, libraries have been created to abstract over them. The three most common of these abstraction libraries are [test-framework](https://hackage.haskell.org/package/test-framework), [Tasty](http://documentup.com/feuerbach/tasty) and [HSpec](http://hspec.github.io/). In this application I'll be using HSpec.


Our first test is a classic unit test, to ensure that both titles and descriptions appear when we `show` a `Location`. HSpec abstracts HUnit's syntax, so we have functions like `shouldBe` in place of `assertEquals`. HSpec also requires that all test-modules' names end with `Spec`

So create a file `test/VForth/LocationSpec.hs` with the following contents

```
module VForth.LocationSpec where

import Test.Hspec
import VForth

newLocation :: String -> String -> Location
newLocation titleText descText = Location {
    title = titleText
  , description = descText
  }

spec :: Spec
spec = do
  describe "Location Display" $ do
    it "Show puts title before dashes before a description" $ do
      show (newLocation "My Title" "The complete description.") `shouldBe` "My Title\n--------\nThe complete description."

    it "Title should be included in showable output" $ do
      show (newLocation "My Title" "The complete description.") `shouldContain` "My Title"

    it "Description should be included in showable output" $ do
      show (newLocation "My Title" "The complete description.") `shouldContain` "The complete description."

```

The indentation is crucial for reading and compiling this. Use the `description` block to identify a test group, and the `it` function to say what each test is doing and provide a HUnit style unit-test. The full list of "shouldXXX" functions are listed on [HSpec's Hackage page](https://hackage.haskell.org/package/hspec-expectations-0.7.2/docs/Test-Hspec-Expectations.html).


Edit `test/Spec.hs` to add the Location test:

```
import Test.Hspec
import qualified VForth.LocationSpec as LocationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Location" LocationSpec.spec
```

Note that the `do` block in the `spec` function can have several `describe` calls one after the other enumerating different tests.

There are two further things to note here. The first is the absence of a `module` header at the top of the file: this is necessary to run the test through Cabal; which is silly, yet true.

The second is that, in order to run the location test, you need to edit your cabal file, and change the to `exposed-modules` parameter of the `library` to:

```
  exposed-modules:     VForth,                
                       VForth.Location
```

Unfortunately, for every module you test, you'll need to "expose" it here: it's not enough to re-export it from the VForth module.

To run this test, type

```
cabal test
```

on the command-line. 

> `ghc-mod` appears not to handle test-dependencies particularly well: you may find it, and therefore Atom, complain about a missing Test.HSpec pacakge until `cabal test` is successfully executed on the command-line

It is tedious that every tested module needs to be listed in Cabal. In particularly, if you fail to expose it, you'll encounter rather strange linking errors, rather than straightforward messages. One cheat is just to include the source of the library in your test app by changing the _test-suite's_ source directories to test project

```
test-suite ventureforth-test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test,src
  main-is:             Spec.hs
  build-depends:       base >= 4.7 && < 5,
                       doctest >= 0.9 && < 0.11,
                       hspec -any
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```
Note that as we've include the `src` directory, we no longer list our `ventureforth` library as a dependency.

While convenient, this does mean that the test fails to detect if a part of the API has been not been exposed. On the other hand, it's often the only way to write tests for "internal" modules that are not exposed be should be tested. 

To square these two pegs, you can create two test suites, each with different names (e.g. ventureforth-test-external and ventureforth-test-internal), one of which uses the publicly exposed modules of the library, and the other of which uses its source files directly. Typing `cabal test` will execute _both_ test-suites: you can have as many test-suites as you want in a Cabal application..

For the time being, we'll use a single test-suite, testing exposed modules only. 


## The Benchmark

Benchmarks are similar to tests: we benchmark exposed modules, we create a benchmark suite for each module we're interested in, and we run all benchmarks from a single `main` program. 

So lets get started by creating the file `bench/VForth/LocationBench.hs`

```
module VForth.LocationBench where

import Criterion.Main
import VForth

benchmarks :: [Benchmark]
benchmarks = [ bench "show" (whnf show l) ]
  where l = Location {
                title="My Title"
              , description = "This is the description."
              }
```

It's worthwhile to look at the [criterion tutorial](http://www.serpentine.com/criterion/tutorial.html) but in simple terms the function `bench` takes a description of the benchmark and a function to benchmark. We return a list of such functions for each module.

As GHC will try to memoize functions, given the same input, there's a chance that the benchmarked function might only ever be executed the first time it's called. To ensure the function is fully executed on every iteration, you can use either the [`nf` or `whnf` functions](https://hackage.haskell.org/package/criterion-1.1.0.0/docs/Criterion-Main.html#g:3) to reduce your function to normal or weak-head-normal forms respectively. WHNF is the Haskell default.

If you have a function that takes more than one argument, you will need to saturate all but the last parameters, e.g.

```
bench "Adding Three numbers" (whnf (addThree 1 2) 3))
```

The [tutorial](http://www.serpentine.com/criterion/tutorial.html) explains these further, including how to handle I/O.

Next, edit `bench/Bench.hs` to add your benchmark tests. As with unit-tests, you must _not_ include a module header in order for Cabal to launch it.

```

import Criterion.Main
import qualified VForth.LocationBench as LocationBench

main :: IO ()
main = defaultMain [
  bgroup "Location" LocationBench.benchmarks
  ]
```

Similar to unit-tests, benchmarks can be grouped, where each group has a label. 

You can run this benchmark using `cabal bench`, which will dump some timings to the console. However realistically graphs are necessary to properly investigate benchmarks, so instead try the HTML output from criterion

```
mkdir -p dist/bench
cabal bench --benchmark-options="--output dist/bench/index.html"
```

In practical terms, it's not necessary to benchmark every function, but it is good to keep track of important functions, and the benchmarking facility is useful if attempting to make changes for reasons of performance.

## Conclusion

So we've created a module, a test and a benchmark. You can

 * Build the application using `cabal repl`
 * Test the module using `cabal test`
 * Benchmark important module functions using `cabal bench`
 * Generate code documentation using `cabal haddock`
 * Build the application using `cabal build`


In the [next part of this tutorial](fixme) we'll look at better ways to create tests using QuickCheck, and a few other minor improvements. After that the focus will turn (at last!) to the application code. 
