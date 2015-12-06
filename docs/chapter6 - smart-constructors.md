# Venture Forth: Smart Constructors

This is the sixth part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part concluded our explanation of [how to layout modules and avoid name clashes](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap6)

In this part of the tutorial we'll

  * Discuss the dangers of "stringly typed" values, and how to avoid them
 * Quickly introduce how one uses regular expressions in Haskell
 * Discuss how to use private constructors, safe constructor functions, and constructor-views to ensure that all types contain only valid data.
 
## Using the Type System

Haskell makes it very easy to create new types, either as `data` declarations ore as `newtype` declarations. This is matched with a  strong and flexible type-system. This simplicity leads to an effective functional programming strategy: encoding as much of your validation and business logic as possible in your types. While Haskell cannot check business logic, it can check types, and if you use types properly, the compiler will catch a lot of bugs for you.

In [the previous part of this tutorial](fixme) we looked at an address type:

```
data Address = Address (
    addrName :: Text
  , addrNumber :: Int
  , addrStreets :: [Text]
  , addrCity :: Text
  , addrCountry :: Text
  , addrPostCode :: Text
  ) deriving Show
```

This is an example of a "stringly-typed" value, we're using strings - random, unbounded, possibly empty sequences of characters - to represent house names, post-codes and other specific values. The upshot is any function that accepts an `Address` object cannot assume it's actually a valid Address.

Any time you see a type with a bunch of strings, you should be suspicious. A better approach, is to use abstract types, like so:

```
newtype AddressFragment = AddressFragment Text deriving Show
type HouseName   = AddressFragment
type StreetName  = AddressFragment
type CityName    = AddressFragment
type CountryName = AddressFragement

data Country =
    UnitedKingdom
  | Ireland
  | France
  | UnknownCountry CountryName
  deriving Show

newtype PostCode = PostCode Text deriving Show

data Address = Address {
    addrName :: HouseName
  , addrNumber :: Int
  , addrStreets :: [StreetName]
  , addrCity :: CityName
  , addrCountry :: Country
  , addrPostCode :: Maybe PostCode
  } deriving Show
```

The final two steps are as follows:

 * Export the types HouseName, StreetName, CityName, Country and Address, but _not_ their data-constructors.
 * Define "smart-constructor" functions which will validate the input, and construct the types, and export _these functions only_.

Now any function that accepts an address can assume it's a valid address. In fact we can now view our application as having two layers: an inner layer of business logic which only use validated abstract types, and an outer layer which does IO with raw text.
 
Two disadvantages exist: the first is that with the constructors no longer visible, we can no longer do pattern-matching based on types (or use the `RecordWildCards` extension). We'll look into this later. The second is that we can no longer use the `Text` functions on our types. The solution to that is to simply allow one to extract text from them, e.g.

```
newtype PostCode = PostCode { postCodeText :: Text } deriving Show
```

or, more idiomatically, have them implement `TextShow` so you can just use `showt` to conver them to `Text`.

## Haskell Regular-Expression Support

The easiest way to validate text is using a regular expression. In this section we'll discuss how to use the regular expression engine in the `text-icu` package to perform regular expressions on `Data.Text` values. If you **want to perform regular expression matching on plain Haskell Strings**, the appendix at the bottom of this page explains that in detail.

The [text-icu](http://hackage.haskell.org/package/text-icu) package provides support for more advanced text manipulations, including unicode normalisations, locale-sensitive word and line-breaking, and regular expresssions. It relies on the C libraries icuuc, icui18n, and icudata, which you'll need to have installed. Linux users can use their package manager. Mac users will probably need to use Homebrew to install the icu4c package, and then direct Cabal to the libraries' locations:

```
brew install icu4c
cabal install --only-dependencies \
	--extra-include-dirs=/usr/local/opt/icu4c/include \
	--extra-lib-dirs=/usr/local/opt/icu4c/lib
``` 

Text-ICU's [regular-expression documentation](http://hackage.haskell.org/package/text-icu-0.7.0.1/docs/Data-Text-ICU.html#g:10) is pretty good. It all relies on a pair of regex constructors

```
regex  :: [MatchOption] -> Text -> Regex
regex' :: [MatchOption] -> Text -> Either ParseError Regex
```
and a pair of finding functions

```
find    :: Regex -> Text -> Maybe Match
findAll :: Regex -> Text -> [Match]
```

The available match-options includes the usual things like case-insensitivity, mult-line support, and line-spanning dot support. As you might expect, `regex` will throw an error if you supply an invalid regex, so if you're accepting unverified input (a risky thing to do) you'll likely prefer `regex'`. 

Once you have a match, you can extract the capture groups using

```
groupCount :: Regular r => r -> Int
group      :: Int -> Match -> Maybe Text
```

where both compiled `Regex` values and `Match` values are instances of `Regular`.


## Validating our Locations and Items

### Title and Description Types

So now to put all this into action. [Back when we were talking about unit-tests](fixme) we found a bug in our `Location` type,  which is that it is possible to construct a `Location` with no title and / or no description.

So now we rectify this, but where to put our validation logic? A reasonable approach is to continue to use `Text` to represent our title and description, and create a smart constructor for our Location type. However since our `Item`s also use a title and description, and are susceptible to the same issues, we're going to create a special module for text, specifically containing `Title` and `Description` types, called `VForth.GameText`. We'll also create a separate module, `VForth.Error`, which will contain our validation errors, and ultimately all errors our application may encounter.

So having created the files, added them to our list of exposed modules in our cabal file, and then exposed them in our top-level `VForth` library, we can add code to them. The `VForth.Error` module is pretty easy for the time being

```
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  The full list of all possible errors that may occur within the VForth
  application
-}
module VForth.Error (
  Error(..),
  errorDescription
) where

import Data.Text (Text)
import Data.Monoid ((<>))

data Error =
  InvalidGameText { msg :: Text }

errorDescription :: Error -> Text
errorDescription InvalidGameText{..} =
  "The game data is corrupted: there as a " <> msg

instance Show Error where
  show = Text.unpack . errorDescription
```

As more and more errors are added, this file will obviously grow. 

The contents of the `VForth.GameText` module is a bit more intesting. We define two types, `Title` and `Description`, and constructor functions `title` and `description` respectively. For brevity we'll discuss only title here.

First we define a generic validation function checking values against minimum & maximum lengths, and against a regular-expression matching _invalid_ characters.

```
validateText :: Int         -- ^ The minimum length (inclusive)
             -> Int         -- ^ The maximum length (exclusive)
             -> Regex       -- ^ A regexp matching invalid characters
             -> (Text -> a) -- ^ A function converting the text to the output type
             -> Text        -- ^ A label for the value used in errors
             -> Text        -- ^ The text to validate and convert
             -> Either Error a
validateText minLen maxLen badCharsRe constructorFunc textDesc rawText =
  let
    t = Text.strip rawText
  in
    if minLen > 0 && Text.null t
    then
      Left . InvalidGameText $ textDesc <> " which is empty"
    else if Text.length t < minLen
    then
      Left . InvalidGameText $ textDesc <> " is too short"
    else if Text.length t > maxLen
    then
      Left . InvalidGameText $ textDesc <> " is too long"
    else
      let
        badChars = findAll badCharsRe t
      in
        if (not . null) badChars
        then
          Left . InvalidGameText $ textDesc <> " which contains the invalid character sequences: " <> delimMatches "; " badChars
        else
          Right . constructorFunc $ t
```

We can then use this to validate our titles

```
newtype Title = Title { titleText :: Text } deriving Show

title :: Text -> Either Error Title
title =
  let
    badChars = regex [CaseInsensitive] "[^a-z0-9 ,.:;£$\\-!?&()']+"
  in
    validateText 3 30 badChars Title "short-text"

instance TextShow Title where
  showb = Bldr.fromText . titleText
```

Note that we've defined a `titleText` function to convert a `Title` value back to a plain `Text` value. In the module exports we export the type, the attribute functions, but _not_ the data-constructor. Instead we export the validating constructor function we declared.

```
module VForth.GameText (
  -- * Titles & Descriptions of items and locations
    Title -- note the absence of (..)
  , title -- smart constructor
  , titleText -- needs it's own export now there's no (..)
  , Description
  , description
  , descText
) where
```

If you look at the [full file on GitHub](fixme) you'll see that it contains full Haddock documentation complete with doc-tests.

Next we need to amend our `Location` and `Item` types. Again for brevity, we'll just focus on `Location`. 

### Location Constructors

In the case of Location, once we change the data-declaration to

```
data Location = Location {
    locTitle :: Title
  , locDescription :: Description
  , locItems :: [Item]
  }
``` 

it becomes impossible to construct an invalid `Location` object. Therefore there is no need to hide the data-constructor and replace it with a constructor function. This means that we can still use the `RecordWildCards` extension with Location objects. That said, we could still define a `location` smart-constructor to maintain the idiom

Once this change is made, you will need to change the `TextShow` implementation, since `locTitle` and `locDescription` are no longer of the `Text` type anymore. Just replace expressions such as `Bldr.fromText locTitle` with `showb locTitle` (recall `showb` converts to a text builder, while `showt` converts to `Text`).

The change to `Item` is almost identical.

### Unit Testing and QuickCheck

There are a number of changes you'll need to make to the unit tests. At a trivial level, you'll need to make small changes such as replacing `locTitle` with `showt locTitle`.

QuickCheck is much more complex however. You'll need to define `Arbitrary` instances for `Title` and `Description`. So create a test module `VForth.GameTextSpec`.

This will have no actual tests, so it's spec implementation is just

```
spec :: Spec
spec = return ()
```

For the arbitrary instance, the orphan restriction means we're once again using testable variants:

```
newtype TestableTitle = TestableTitle Title deriving Show
```

The next question is how to implement this instance. A simple approach is to use the `suchThat` function which filters out certain instances. Such an implementation for `title` would be:

```
right :: Show l => Either l r -> r
right (Right r) = r
right (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

instance Arbitrary TestableTitle where
  arbitrary =
    let
      anyText   = arbitrary
      validText = suchThat anyText (isRight . title)
    in
      (TestableTitle . right . title) <$> validText
```

The issue with this approach is that _huge_ numbers of generated inputs are discarded. For `Title`, this is merely very slow. For `Description` however, which requires at least 30 valid characters, the program will take hours to generate enough valid instances. As a result, we unfortunately have to couple our testing code with our tested code, and manually write an arbitrary instance

```
instance Arbitrary TestableTitle where
  arbitrary =
    let
      validChars  = elements (['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ",.:;£$-!?&()' ")
      validString = choose (3, 30)
                    >>= \count -> replicateM count validChars
      validText   = Text.pack <$> validString
    in
      (TestableTitle . right . title) <$> validText
```

The `elements` function takes a list of possible values, and turns it into a generator which will return a single value selected at random from that list at each call. We can generate `n` such characters by calling `replicateM` (recall arbitrary values live in the `Gen` Monad). However we also want to select `n` at random too, and have it be at least 3 and no more than 30. This is why we use the `choose` function.

The definition of `Description` follows similarly.

This is brittle however, as any change to the validation rules need to be replicated in the change to the test. This can be allevitated slightly by exposing these constraints as constant values in the tested code. 
 
## Conclusions

We have now ensured that it is impossible to create invalid `Location` or `Item` values. We've achieved this by

 * Adding our own types for all values, instead of using unvalidated `Text` etc types
 * Hiding data-constructors and only exposing constructor-functions that validate data first
 
We've also begun to discuss error handling, where we define a single Error module for our library which enumerates all possible errors, and provides functions for describing these to users. Functions which may fail return an `Either Error a` value. 

This seems rather long-winded, and tedious, but in the real-world, you will need to validate your data somewhere. The result is a significant improvement in the safety of our program.

We've also dicussed regular expressions and how they work with the `Data.Text` value. If you want to use regular expressions on normal Haskell `String`s, the appendix below explains this in detail.

The code for this part of the tutorial is [here](https://github.com/budgefeeney/ventureforth/tree/master/chap6).

In the [next part of the tutorial]() we'll make our world interactive, allowing players to move around.

## Appendix: Haskell Regular-Expression Support for Strings

The original Haskell support for Strings is rather complex, and not particularly well documented, so I've provided a write-up here. The first challenge you have is to choose a regular expression engine from one of the [ten available](https://wiki.haskell.org/Regular_expressions#regex-tdfa).

While the [PCRE engine](https://hackage.haskell.org/package/regex-pcre) might seem to be the obvious choice, it has some pecularities due to the overhead of converting Haskell Strings to C Strings on each call. Instead, the best default choice for an engine is the [TDFA engine](http://hackage.haskell.org/package/regex-tdfa). This uses the POSIX syntax, with a [few GNU extensions](https://hackage.haskell.org/package/regex-tdfa-1.2.1/docs/Text-Regex-TDFA.html). POSIX is very similar to PCRE, though it is greedy where PCRE is lazy.

So with that in mind, you'll likely want to add the following two dependencies to your Cabal file

```
regex-base >= 0.93.1
regex-tdfa >= 1.2.1
```

TDFA is built around a Perl style `=~` operator, but since it uses a lot of type tricks to overload this, its [documentation](https://hackage.haskell.org/package/regex-tdfa-1.2.1/docs/Text-Regex-TDFA.html) is not very useful. Here are a few examples that should make things clearer:

```
import Text.Regex.TDFA

mystr = "FooBarBazFud"

mystr =~ "F(oo|ud)" :: Bool 
  == True

mystr =~ "F(oo|ud)" :: String 
  == "Foo"

mystr =~ "F(oo|ud)" :: (MatchOffset, MatchLength)
  == (0, 3)

getAllTextMatches (mystr =~ "F(oo|ud)") :: [String]
  == ["Foo", "Fud"]
    
getAllMatches (mystr =~ "F(oo|ud)") :: [(MatchOffset, MatchLength)]
  == [ (0, 3), (9, 3) ]  
```

Note there are two different functions: `getAllTextMatches` and `getAllMatches`.

Regular expressions are, of course, case-insensitive, so

```
mystr =~ "f(oo|ud)" :: Bool == False
```

To make a case-insensitive regex you need to explicity construct a regex value using the `makeRegexOpts` function and two data-types: [CompOption and ExecOption](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA-Common.html). A useful shortcut is to use the `defaultCompOpt` value as the basis for your new `CompOption` value and likewise for `ExecOption`

```
compOpts = defaultCompOpt { caseSensitive = True }
execOpts = defaultExecOpt { captureGroups = True }

regex = makeRegexOpts compOpts execOpts "f(oo|ud)"

match regex mystr :: Bool 
  == true
  
getAllTextMatches (match regex mystr) :: [String]
  == ["Foo","Fud"]  
  
-- and so on
```

Note that if you provide an invalid regular-expression pattern to `makeRegexOpts`, it will return without any complaint, but the first call to `match` will throw an exception. If you're taking an untrusted text as a regular expression (itself a very dangerous business), you may prefer instead to put everything in a Monad, use the `makeRegexOptsM` function and use `MonadError` to handle the errors, as described on the [RegexMaker hackage entry](http://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html). The simplest approach is just to use the `Either` monad:

```
-- Makes Either an instance of MonadError
import Control.Monad.Except

regex = case makeRegexOptsM compOpts execOpts "f(oo|ud)" of 
			Left err -> error ("Invalid regex: " <> err)
			Right re -> re
```

