# Venture Forth: Smart Constructors

This is the sixth part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part concluded our explanation of [how to layout modules and avoid name clashes](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap6)

In this part of the tutorial we'll

  * Discuss the dangers of "stringly typed" values, and how to avoid them
 * Quickly introduce how one uses regular expressions in Haskell
 * Discuss how to use private constructors, safe constructor functions, and constructor-views to ensure that all types contain only valid data.
 
## Using the Type System

Haskell makes it very easy to create new types, either as `data` declarations ore as `newtype` declarations. This is matched with a  strong and flexible type-system. This simplicity leads to an effective functional programming strategy: encoding as much of your validation and business logic as possible in your types. While Haskell cannot check business logic, it can check types, and if you use types properly, the compiler will catch a lot of bugs for you.

In [the previous part of this tutorial](fixme), as an aside, we looked at an address type:

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
newtype StreetName = StreetName Text deriving Show 
newtype CityName   = CityName Text deriving Show

type PostCode = Text
type CountyName = Text

data CountryCode =
    UnitedKingdom PostCode
  | Ireland (Maybe Postcode)
  | France PostCode
  | UnknownCountry CountryName (Maybe Postcode)
  deriving Show

data HouseIdentifier = HouseIdentifier {
    houseName :: Text
  , houseIdNum :: Int
  , houseIdNumQualifier :: Text
  , flatName :: Text
  , flatIdNum :: Int
  , flatIdNumQualifier :: Text
  }

data Address = Address {
	addrHouseId :: HouseIdentifier
  , addrStreets :: [StreetName]
  , addrCity :: CityName
  , addrCountryCode :: CountryCode
  } deriving Show
```

The final two steps are as follows:

 * Export the types HouseName, StreetName, CityName, CountryCode and Address, but _not_ their data-constructors.
 * Define "smart-constructor" functions which will validate the input, and construct the types, and export _these functions only_.

Now any function that accepts an address can assume it's a valid address. In fact we can now view our application as having two layers: an inner layer of business logic which only use validated abstract types, and an outer layer which does IO with raw text.
 
Two disadvantages exist: the first is that with the constructors no longer visible, we can no longer do pattern-matching based on types (or use the `RecordWildCards` extension). We'll look into this later. The second is that we can no longer use the `Text` functions on our types. The solution to that is to simply allow one to extract text from them, e.g.

```
newtype PostCode = PostCode { postCodeText :: Text } deriving Show
```

or simply have them implement `TextShow` so you can just use `showt` to convert them to `Text`.

## Adding Validation Rules

Haskell has two libraries for validation. The [Validation](https://hackage.haskell.org/package/validation) library is more fully featured, and supports the ability to accumulate errors into a collection (using an appropriate semigroup operator). A simpler library is [Validate Input](https://hackage.haskell.org/package/validate-input). The latter uses PCRE as a backend for regular expressions, which adds a lot of complexity with non-English unicode characters.

Consequently, from a perverse desire for both simplicity and unicode correctness, I've just written [this very simple validation module](fixme). All it does is check the text is within the given length bounds, and constains the given characters. We use the `Data.Default` module to simplify the creation of new `ValidationConstraint` objects.

This only addresses the case of text. It's worth noting, that for numeric quantities, whether it's quantities of cash, or just miles per minute, you should be using the [dimensional](http://hackage.haskell.org/package/dimensional) [[tutorial]](http://conscientiousprogrammer.com/blog/2015/12/20/24-days-of-hackage-2015-day-20-dimensional-type-checked-computation-on-physical-quantities-with-units/) library

Rather than work out how to validate a street-name however, we'll look at validating the titles and descriptions of locations and items.

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

