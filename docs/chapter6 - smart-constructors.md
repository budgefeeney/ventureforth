# Venture Forth: Smart Constructors

This is the sixth part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part concluded our explanation of [how to layout modules and avoid name clashes](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap6)

In this part of the tutorial we'll

  * Discuss the dangers of "stringly typed" values, and how to avoid them
 * Quickly introduce how one uses regular expressions in Haskell
 * Discuss how to use private constructors, safe constructor functions, and constructor-views to ensure that all types contain only valid data.
 
## Using the Type System

Haskell makes it very easy to create new types, either as `data` declarations ore as `newtype` declarations. This is matched with a very strong, clever and flexible type-system. One of the tricks of functional programming however, is to try to encode as much of your validation and business logic as possible in your types, because while Haskell cannot check business logic directly, it can check types, and if you use types cleverly, the compiler will catch a lot of bugs for you.

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
type HouseName  = AddressFragment
type StreetName = AddressFragment
type CityName   = AddressFragment

data Country =
    UnitedKingdom
  | Ireland
  | France
  | UnknownCountry AddressFragment
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

 * Export the types HouseName, StreetName, CityName, Country and Address, but _not_ the constructors. Also export Country.
 * Define functions which will validate the input, and construct the types, and export _these functions only_.

Now any function that accepts an address can assume it's a valid address. In fact we can now view our application as having two layers: an inner layer of business logic which only use validated abstract types, and an outer layer which does IO with raw text.
 
Two disadvantages exist: the first is that with the constructors no longer visible, we can no longer do pattern-matching based on types (or use the `RecordWildCards` extension). We'll look into this later. The second is that we can no longer use the `Text` functions on our types. The solution to that is to simply allow one to extract text from them, e.g.

```
newtype PostCode = PostCode { asText :: Text } deriving Show
```

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
regex :: [MatchOption] -> Text -> Regex
regex' :: [MatchOption] -> Text -> Either ParseError Regex
```
and a pair of finding functions

```
find :: Regex -> Text -> Maybe Match
findAll :: Regex -> Text -> [Match]
```

The available match-options includes the usual things like case-insensitivity, mult-line support, and line-spanning dot support. As you might expect, `regex` will throw an error if you supply an invalid regex, so if you're accepting unverified input (a risky thing to do) you'll likely prefer `regex'`. 

Once you have a match, you can extract the capture groups using

```
groupCount :: Regular r => r -> Int
group :: Int -> Match -> Maybe Text
```

where both compiled `Regex` values and `Match` values are instances of `Regular`.


## Validating our Locations and Items

So now to put all this into action. [Back when we were talking about unit-tests](fixme) we found a bug in our `Location` type,  which is that it is possible to construct a `Location` with no title and / or no description.

So now we rectify this, but where to put our validation logic? A reasonable approach is to continue to use `Text` to represent our title and description, and create a smart constructor for our Location type. However since our `Item`s also use a title and description, and are susceptible to the same issues, we're going to create a special module for text, specifically containing `Title` and `Description` types, called `VForth.GameText`. We'll also create a separate module, `VForth.Error`, which will contain our validation errors, and ultimately all errors our application may encounter.

So having created the files, added them to our list of exposed modules in our cabal file, and then exposed them in our top-level `VForth` library, we can add code to them. The `VForth.Errors` module is pretty easy for the time being

```
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  The full list of all possible errors that may occur within the VForth
  application
-}
module VForth.Errors (
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

The contents of the `VForth.GameText` module is a bit more intesting. We define two types, `ShortDisplayText` and `MedDisplayText`, and then create type-alises called `Title` and `Description` respectively. The reason for the aliasing is simply in case other classes of text (e.g. `PlayerName`) arise that have the same validation criteria as, for example, `ShortDisplayText`. The part of the file dealing with titles is:


```
badCharsForShortRe :: Regex
badCharsForShortRe = 
  regex [CaseInsensitive] "[^a-z0-9 ,.:;£$\\-!?&()]+"

shortDisplayText :: Text -> Either Error ShortDisplayText
shortDisplayText rawText =
  let
    t = Text.strip rawText
  in
    if Text.null t
    then
      Left (InvalidGameText "short-text which is empty")
    else if Text.length t > 30
    then
      Left (InvalidGameText "short-text which is too long")
    else
      let
        badChars = findAll badCharsForShortRe t
      in
        if (not . null) badChars
        then
          Left (InvalidGameText $ "short-text which contains the invalid character sequences [" <> delimMatches ", " badChars <> "]" )
        else
          Right . ShortDisplayText $ t


type Title = ShortDisplayText

title :: Text -> Either Error Title
title = shortDisplayText
```

If you look at the [full file on GitHub](fixme) you'll see that it contains full Haddock documentation complete with doc-tests. The `MedDisplayText` type simply allows longer text-fragments and new-lines
 
## Conclusions

Blah


The code for this part of the tutorial is [here](https://github.com/budgefeeney/ventureforth/tree/master/chap6).

In the [next part of the tutorial]() we'll make our world interactive, allowing players to move around.

## Appendix: Haskell Regular-Expression Support on Strings

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
```

Note that if you provide an invalid value to `makeRegexOpts`, the first call to `match` will throw an exception. If you're taking an untrusted text as a regular expression (itself a very dangerous business), you may prefer instead to put everything in a Monad, use the `makeRegexOptsM` function and use `MonadError` to handle the errors, as described on the [RegexMaker hackage entry](http://hackage.haskell.org/package/regex-base-0.93.2/docs/Text-Regex-Base-RegexLike.html). The simplest approach is just to use the `Either` monad:

```
-- Makes Either an instance of MonadError
import Control.Monad.Except

regex = case makeRegexOptsM compOpts execOpts "f(oo|ud)" of 
			Left err -> error ("Invalid regex: " <> err)
			Right re -> re
```

