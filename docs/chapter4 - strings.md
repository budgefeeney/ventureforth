# Venture Forth: The Dangers of Strings and Stringly Typing

This is the fourth part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part concluded our explanation of [how to layout a project with unit-tests and benchmakrs](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap4)

In this part of the tutorial we'll

 * Discuss alternatives to the `String` type and why they exist
 * Illustrate how to use `Text` instead, and how to efficiently construct `Text` values
 * Discuss the `ShowText` type-class.
 * Discuss the dangers of "stringly typed" values, and how to avoid them
 * Quickly introduce how one uses regular expressions in Haskell
 * Discuss how to use private constructors, safe constructor functions, and constructor-views to ensure that all types contain only valid data.
 
## Better Strings in Haskell

A Haskell String is just a list of characters. If you peruse the Prelude, you'll eventually find something like

```
type String = [Char]
```

What this means is that in practice, a String in Haskell is a linked list of boxed characters, where a boxed character is either a pointer to a heap-allocated character, or a pointer to a thunk -- an unevaluated code-fragment -- which will on access will execute and return a character to be stored on the heap. 

While a nice teaching example, this is horribly inefficient, both in terms of storage and in terms of the usual string operations. Most languages typically hide how they store and represent characters (which is [not trivial](http://kunststube.net/encoding/)) and present a API to access them. Haskell has two alternative String types which adopt this approach, and promise more efficient storage and fast read-only access.

### Data.ByteString

[`Data.ByteString`](https://hackage.haskell.org/package/bytestring) is a simple packed representation of bytes with compact storage and fast access. It is totally unconcerned with what sort of data those bytes represent: they could be pixels in an image, a fragment of a music file, or representations of characters in a String.

If your ByteString contains text using _only_ those characters from subset of Unicode covered by code points 0-255, i.e. Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls, encoded in UTF-8, then you can use the [`Data.ByteString.Char8`](https://hackage.haskell.org/package/bytestring-0.10.6.0/docs/Data-ByteString-Char8.html) package to work with a sequence bytes as if they were characters in a string. This will work for English and some Western European languages, but will break as soon as any other character arises. 

There is a [UTF-8 extension to ByteString](https://hackage.haskell.org/package/utf8-string), but it is limited in scope.

The `Data.ByteString.Char8` module provides `pack` and `unpack` methods to convert `String`s to `ByteString`s and vice-versa. 

Because `ByteString` funtions have the same names as normal `String` functions in the prelude, it needs to be imported with a qualification, e.g.

```
import qualified Data.ByteString as BS
```

However with the rise of emoticons, not even English speakers regularly use characters 0-255 anymore. For example this code snippet operates on a valid UTF-8 encoded string:

```
BS.unpack $ BS.pack $ "H\x00C9llo \x1F4A9"`
```
However it is a _destructive_ operation, returning a different string to its input: `"H\x00C9llo \x00A9"`.


### Data.Text

Therefore the better alternative, and one you should always use by default, is [`Data.Text`](https://hackage.haskell.org/package/text). This can be thought of as encapsulating a `ByteString` paired with an encoding specifying how its bytes should be mapped to characters.

Text uses UTF-16 to represent characters. For English text, this is wasteful compared to UTF-8, so you will see some people recommending `ByteString` over `Text` for improvements in memory usage and speed. This should always be understood to be an optimization of last resort, trading safety for performance. Moreover, while there are certainly [cases where UTF-8 makes more sense than UTF-16](http://www.utf8everywhere.org/#asian), in practice the [improvements are often not as great as might be expected](http://jaspervdj.be/posts/2011-08-19-text-utf8-the-aftermath.html).

As with `ByteString`, you will need to to use a qualified import of `Data.Text` to avoid name-clashes with the prelude.[[1]](fixme)

```
import qualified Data.Text as T
```

Both Text and ByteString are strict by default: if you want lazy behaviour (e.g. not loading an entire file into memory at once), use their Lazy variants (e.g. [`Data.Text.Lazy`](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy.html)).

As with ByteStrings, the Text module provides `pack` and `unpack` methods to convert `String`s to `ByteString`s and vice-versa.

It also provides many String functions (such as `trim`) that are not included in the prelude, as well as replacements for all prelude functions on Strings and also on lists (e.g. `replicate`, `isInfixOf` etc.). This page has [most of the Text functions](https://hackage.haskell.org/package/text/docs/Data-Text.html)

Finally, if you want to use some more sophisticated string manipulations, including unicode transformations, you should consider using the [text-icu](http://hackage.haskell.org/package/text-icu) library.


### Overloading Strings

Since calling `pack` and `unpack` repeatedly is tedious, GHC has an extension called `OverloadedStrings` which performs these `pack`/`unpack` transformations on String literals automatically. Just put

```
{-# LANGUAGE OverloadedStrings #-}
```

at the top of your file. Note that this does _not_ work with variables or the results of functions, where you will still need to call pack/unpack explicitly.

### Implementing Location with Data.Text

So, the first thing we'll be doing is converting our `Location` module to use Text. We'll investigate the cost of this transition along the way, so the first thing to do is run `cabal bench` on your current implementation and take a note of how long `length . show` takes on average.

Now to the conversion. First add `text >= 1.2` as a build dependency of the ventureforth library, then run `cabal install --only-dependencies` to ensure it's downloaded. Next edit the `Location.hs` file so it becomes:

```
{-# LANGUAGE OverloadedStrings #-}
module VForth.Location (
   Location(..)
 ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Monoid ((<>))

data Location = Location {
    title :: Text
  , description :: Text
  }

instance Show Location where
  show l = T.unpack $
    title l <> "\n"
    <> T.replicate (T.length $ title l) "-" <> "\n"
    <> description l
```

There are a few things to note here. First note the two-line Text import. While opinions vary, my preference, and one I've seen used elsewhere, is to import type and constructor names _without_ qualification, then import the associated functions _with_ qualification.

The next thing is that we're replaced the `++` operator, which only operates on lists, with the more generic `<>` operator, which works on any kind of combinable thing (i.e. any [Monoid](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids)). In fact, to keep your code as generic as possible, you should prefer `<>` and `fmap` (or it's operator `<$>`) over the list-specific `++` and `map`.

Lastly note that we have to explicitly call `T.unpack` to convert to convert to the desired `String` type. This is obviously wasteful, and we'll address it later in this tutorial.

Our unit-test similarly needs to be modified, but first we need to add some dependencies to our build depends section: `text >= 1.2` and `quickcheck-text >= 0.1`. The latter package provides an instance of `Arbitrary` for `Data.Text`. You'll need to download these, so run

```
cabal install --enable-test --only-dependencies
```

To modify the unit test, just put `{-# LANGUAGE OverloadedStrings #-}` at the top of the file and add the following additional imports

```
import qualified Data.Text as T
import Data.Text.Arbitrary
```

Next replace all instances of `null` with `T.null` and make sure to use `T.unpack` to convert text to String[2] in your test comparisons, e.g.

```
    it "Description should be included in showable output" $ property $
      \(TestableLocation l) -> (T.unpack $ description l) `isInfixOf` show l

    it "Showable output is never blank"  $ property $
      \(TestableLocation l) ->
        (not . T.null . title $ l) && (not . T.null . description $ l)
        ==> any (not . isSpace) (show l)
```

Having changed all four tests, the last change is in the benchmark: here all you need to do is add `{-# LANGUAGE OverloadedStrings #-}` to the top of the file. Run the tests with `cabal test`, then run the benchmarks with `cabal bench`

This is where the bad news starts, on my machine the `length.show` implementation with `Text` is _slower_ than the old `String` version.

So what on earth is causing this?

The reason is strictness: while it's efficient for memory usage and read-access, every call to `<>` on a `Text` type constructs a new type in memory, and discards the old ones. Thus we are constantly allocating, copying, and releasing memory as we build up our value.

The solution is to use a [text builder](https://hackage.haskell.org/package/text/docs/Data-Text-Lazy-Builder.html) to iteratively build up a lazy `Text` object, and then convert it to a _strict_ `Text` object only when we need it. This is similar to the use of classes such as `StringBuilder` in Java or C#.

So add the following imports to the top of your `Location` file:

```
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as Bldr
```

and then change the `show` implementation to use a builder:

```
instance Show Location where
  show l =
    let
      titleLen = fromIntegral (T.length (title l))
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
    in
    LT.unpack
    $ Bldr.toLazyText
    $ Bldr.fromText (title l) <> sep <> Bldr.fromText (description l)
```

The benchmark should now be faster than the plain String. If you look at the [code associated with this part of the tutorial]() you will see a second benchmark suite included in `bench/VForth/TextBench.hs` which compares Strings, Text, Text with Builders and ASCII ByteStrings using short, medium and long inputs. For medium and long, Text with Builder beats String. ByteString beats all of them, but you must accept your program will have undefined, destructive behaviour if your text input is not Latin-1 encoded.

## The TextShow Typeclass

An issue with what we've discussed so far is we use `Text` internally to represent text values, but then convert them to `String`s for IO. This is unnecessary. The `Data.Text.IO` module provides `putStrLn` and similar functions operating on `Text`. So we just need to implement a variant of the `Show` type-class which returns `Text` values instead of `String`s and we can eliminate the need for a conversion entirely.

Fortunately we don't need to create this typeclass ourselves, a definition already exists, with implementations for most of the built-in types. It's the [text-show](http://hackage.haskell.org/package/text-show) library.

Changing our code is pretty easy, just `import TextShow`, and replace occurrences of `show` with `showt`, `print` with `printT` (note the case) and the typeclass `Show` with the typeclass `TextShow`. Note that the result of `TextShow` is a _builder_, rather than a `Text` object, so our `TextShow` implementation for `Location` simplifies to:

```
instance TextShow Location where
  showb l =
    let
      titleLen = fromIntegral (T.length (title l))
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
    in
    Bldr.fromText (title l) <> sep <> Bldr.fromText (description l)
```

Don't forget to update your cabal file to include the `text-show` dependency as well. The chances to the benchmarks and unit-tests I'll leave as an exercise.

## Conclusion

In this section we've

 * Demonstrated that `String`s perform worse than `Data.Text` and `ByteString` for most operations.
 * Discussed how to use a `Builder` to efficiently construct a `Text` value
 * Shown how to use the `TextShow` typeclass instead of `Show`
 * Converted our `Location` type to use `Text` and `TextShow`

The code associated with this part of the tutorial is [here](fixme). 

In the [next part of this tutorial](fixme) we'll create an additional type to represent `Item`s in a location that a user can pick up, address a problem of name-clashes between types, and look into some useful Haskell extensions to alleviate this.

-----

1. There is a project called Classy Prelude ([github](https://github.com/snoyberg/classy-prelude), [hackage](https://hackage.haskell.org/package/classy-prelude)) which replaces the standard prelude, which is almost 20 years old, with a newer, safer, more modern implementation using modern Haskell idiomata. One of the improvements is that 'Text' is used by default as the String type insofar as possible. 
2. You may have discoverd that you can use `show` to convert `Text` to a `String`, but the resulting string has all special characters escaped (as if for inclusion in Haskell code), and so is not suitable for human-readable output.	