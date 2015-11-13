# Venture Forth: Better Tests

This is the third part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part discussed [how to write a module with a test and benchmark](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap3)

In this part of the tutorial we'll

 * Look into creating better tests with [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.4.2) [(tutorial)](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing)
 * Look into how to automatically include our test "Spec" files
 * Look into how [doctest](https://hackage.haskell.org/package/doctest) can test our documentation
 
This will complete the walk-through of how to test Haskell programs. The [next part](fixme) of the tutorial will turn our focus to _writing_ Haskell programs.
 
## QuickCheck

The classic approach to unit-testing is that the developer writes a few tests, with a few examples they've thought of, then writes the code.

The problem with this approach is that bugs arise when the assumptions developers make fail to hold in practice; and these assumptions carry over from the code the developer writes to the examples they chose to test it on. A much better way to write tests therefore is to specify some property that the code should have, and then have an automated procedure generate a hundred or so possible random inputs.

This is exactly what QuickCheck does. You specify a property, and it generates the examples. As [explained here](https://www.fpcomplete.com/user/pbv/an-introduction-to-quickcheck-testing), Quickcheck requires datatypes to be instances of the  `Arbitrary` typeclass, so it can generate examples. There are built-in implementations of `Arbitrary` for the simple base types like `Int`, `String` etc, so we use these to implement `Arbitrary` for our location:

```
newtype TestableLocation = TestableLocation Location
	deriving Show
	
instance Arbitrary TestableLocation where
	arbitrary = do
		titleText <- arbitrary
		descText  <- arbitrary
		return $ TestableLocation $ newLocation titleText descText
```

Why did we not implement this for `Location`? Simply put, if the implementation is not in the same module as the data-type, GHC issues a warning about ["orphan types"](http://stackoverflow.com/a/3079748/2722784). This is to help avoid the situation where the same typeclass is instantiated for a type in multiple locations. However since we don't want to clutter our business logic with test-logic, we wrap the datatype in a newtype, and have the newtype implement the typeclass in our test module.

Note that QuickCheck requires all samples to be `Show`-able.

As you can see this happens in a monad: this is because `arbitrary` returns `Gen a` for a type `a`. You don't really need to use monads however, it's often simpler just to use an applicative functor:

```
instance Arbitrary TestableLocation where
  arbitrary = TestableLocation <$> (
  	newLocation <$> arbitrary <*> arbitrary
  	)
```

Putting this together, we can replace our previous hard-coded unit tests detecting the presence of title and description in the show-ed text with QuickCheck properties:


```
module VForth.LocationSpec where

import Data.List (isInfixOf)
import Data.Char (isSpace)
import Test.Hspec
import Test.QuickCheck
import VForth

newtype TestableLocation = TestableLocation Location deriving Show

instance Arbitrary TestableLocation where
  arbitrary = TestableLocation <$> (
    newLocation <$> arbitrary <*> arbitrary
    )

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

    it "Title should be included in showable output" $ property $
      \(TestableLocation l) -> title l `isInfixOf` show l

    it "Description should be included in showable output" $ property $
      \(TestableLocation l) -> description l `isInfixOf` show l

```

Note that while HSpec completely abstracts HUnit, it does not do so for QuickCheck, which is why we've had to import `Test.QuickCheck`. If we compare the last of the three tests with its previous HUnit implementation, we can see the differences are slight:

```
  it "Description should be included in showable output" $ do
      show (newLocation "My Title" "The complete description.") `shouldContain` "The complete description."
```

In short, all that happened was the `do` at the end of the first line line was replaced with a `property $` (without a do) and immediately followed by an inline function definition. This in turn takes a `TestableLocation` and evaluates a boolean function, a property, which should always return `True` for correct implementations.

Try running this using `cabal test`. It should pass. Now lets try adding another QuickCheck property: in this case that the output for `Show` should never be blank, i.e. it should contain at least one visible character:

```
it "Showable output is never blank" $ property $
      \(TestableLocation l) -> any (not . isSpace) (show l)
```

If we run this using `cabal test` we get an test failure. At first glance it doesn't look very helpful

```
  test/VForth/LocationSpec.hs:37:
  1) Location, Location Display, Showable output is never blank
       Falsifiable (after 1 test): 
       TestableLocation
```

However if you look closely, we can see it has actually printed out the example that failed, it's

```
TestableLocation
```

Obviously our `Location` is blank! This brings up a second reason for wrapping our test modules in a newtype: we can create a less user-friendly and more "debuggable" `Show` implementations. If we add the this to `LocationSpec.hs`

```
instance Show TestableLocation where
  show (TestableLocation l) = "Location { title=\"" ++ title l ++ "\", description=\"" ++ description l ++ "\" }"
```

and run the test again, we now see exactly what's happening:

```
  test/VForth/LocationSpec.hs:35:
  1) Location, Location Display, Showable output is never blank
       Falsifiable (after 1 test): 
       Location { title="", description="" }
```

QuickCheck has indeed found a failing case: if the title is blank, and the description is too, then so too is the output of `show`. To my mind this is actually a genuine bug: a `Location` with a blank title and description is a nonsense. In order for our types to have meaning, we should ensure that types can only be created with valid data.

We'll address in the [next part of the tutorial](fixme), but for now we'll add a pre-condition to our QuickCheck test to ensure that it's only evaluated when the title and description are not empty. To do this, we use QuickCheck's `==>` operator, where a precondition on the left must evaluate to true in order for the property on the right to be tested.

```
    it "Showable output is never blank"  $ property $
      \(TestableLocation l) ->
        (not . null . title $ l) && (not . null . description $ l)
        ==> any (not . isSpace) (show l)
```

## Finding Tests Automatically

Our application still has just the one test-spec, but we'll be adding more as our tutorial proceeds. Recall that in the previous chapter we wrote a test-launcher application, `test/Spec.hs`, as 

```
import Test.Hspec
import qualified VForth.LocationSpec as LocationSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Location" LocationSpec.spec
```

It is not only tedious to manually import each and every test-spec, and append a corresponding `describe` call to the `do` block in `main`: it's risky. It's very easy, through accident, to either delete a test's inclusion or never to have included it at all. 

A far better approach would be to have a program recursively scan the `test` directory, find all files ending in `*Spec.hs` and use them to create a `main` function for us. Such a tool exists: it's called `hspec-discover`.

If we delete everything in `test/Spec.hs` and instead just write the single line

```
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

then when run `cabal test`, GHC will be passed the command-line flags `-F -pgmF hspec-discover` when it tries to compile `test/Spec.hs`, which will cause it insteaad to compile the output of `hspec-discover`. The resulting program will execute all our `*Spec.hs` files.

A similar functionality is not available for benchmarking, but in general you won't have as many benchmarks as tests, so this is less of an issue.

## Documentation Tests

As the final part of our test harness, we'll incorporate [doctest](https://hackage.haskell.org/package/doctest). Doctest is a port of a [Python library](https://docs.python.org/3.5/library/doctest.html) of the same name, which 
 
 * Checks that code samples in Haddock comments compile
 * Verifies that the output of those snippets matches the output given in the comment.
 
There are two reasons for doing this

 1. It keeps simple tests close to the code they're testing, instead of in a separate test-spec file in the test-suite
 2. It encourages us to provide lots of code examples in our documentation. Haskell developers often do a poor job of this: compare the documentation, generated from comments, for [Haskell's Maybe type](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html) with [Rust's equivalent Option type](https://doc.rust-lang.org/std/option/enum.Option.html).
  
Since at present we don't have any good candidates, we'll just add a silly little method to `src/VForth.hs`, our library entrypoint, to check if a username is valid or not.

```
module VForth (
   module VForth.Location
 , welcomeMsg
 , isValidUserName
 ) where

import VForth.Location
import Data.Char(toUpper)

-- | The message shown when the application starts
welcomeMsg :: String
welcomeMsg = "Wake up! It's time to venture forth"

{- |
Checks if the given username is valid. A valid username contains Latin alphabetic
characters only, and is at least three letters long. This is not case-sensitive.
As an example, "Tony" is a valid name

>>> isValidUserName "Tony"
True

However names featuring digits, accents or even spaces are not allowed.

>>> map isValidUserName [" Tony ", "Tony1987",  "Tóni", "Ton", ""]
[False, False, False, False, False]
-}
isValidUserName :: String -> Bool
isValidUserName uname =
  let
    isAllAlpha = and . map (`elem` ['A'..'Z']) . map toUpper
  in
    (length uname > 3) && (isAllAlpha uname)

```

As you can see we're using the longform Haddock comment with `{- |` and `-}`. The doctest code is denoted by `>>>` and the expected output is provided on the subsequent line. [Oliver Charles's introduction to doctest](https://ocharles.org.uk/blog/posts/2013-12-18-doctest.html) has examples of how to construct more complex code examples, including let-blocks, IO, and multiline outputs.

Observe that the code samples in the example above make it really easy for a user to understand what a valid username does and does not look like, and it provides us with a bunch of unit-tests for free.

So how do we execute them?

Well, we need to create an entirely new test-suite. Open up your `ventureforth.cabal` file and add the following additional test-suite section:

```
test-suite ventureforth-doctest
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test,src
  main-is:             DocTest.hs
  build-depends:       base >= 4.7 && < 5,
                       doctest >= 0.9 && < 0.11,
                       Glob >= 0.7.5
  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

Note that this test-suite directly accesses the source, and therefore internal implementaiton details, of our Venture Forth library. 

Now create the file `test/DocTest.hs` with the following content:

```
module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "src/**/*.hs" >>= doctest
```

This will load _all_ our source files and execute any doc-tests it finds within them. I should note this is just copied from [Taylor Fausak's Haskeleton template](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/#documentation-tests).

Type `cabal test` and now both test-suites will be run. 


## Conclusion

So we have, at great length, discussed how to structure a project, and test its code. It's worth nothing, that in practice, you wouldn't do all this manual work yourself. Instead using the [Haskell Init tool](https://github.com/fujimura/hi), all you would need to do is type

```
hi ventureforth \
	--repository git://github.com/tfausak/haskeleton.git \
	--package-name ventureforth \
	--module-name VForth \
	--author Bryan Feeney \
	--email bryan.feeney@mailserver.com
```
and it would create the project skeleton with HSpec testing, doctest testing, criterion benchmarking and the usual library / executable layout, according to the Haskeleton template ([tutorial](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/)) as specified by the repository flag. Other [HI template repositories](https://github.com/fujimura/hi/wiki#available-templates) for web-apps etc. exist.

All that would then be left to do is to create the `Location.hs` and `LocationSpec.hs` files.

The Haskell Init tool can be installed globally by simply typing

```
cabal install hi
```

in a directory which does not contain a sandbox.

In the [next part of this tutorial](fixme), we will, at last, talk about writing application code. In particular, we'll address our test-failure (nonsense Location objects), and look into how to store strings in Haskell.


