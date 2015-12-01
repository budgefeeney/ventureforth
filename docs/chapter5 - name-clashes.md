# Venture Forth: Name Clashes

This is the fifth part of the [Venture Forth tutorial](fixme) on how to create a full Haskell application. The previous part concluded our explanation of [how and why one should use `Text` instead of `String`s](fixme). Code for this part can be found [on github](https://github.com/budgefeeney/ventureforth/tree/master/chap5)

In this part of the tutorial we'll

 * Create a second module
 * Dicuss how to work around cases where name clashes arise (E.g. two types with a "title" attribute)
 * Look at some extensions to make such workarounds easier
 

## A Useful Extension

### RecordWildCards

In our original `Show` implementation for `Location` we had to use functions to access the location properties, something similar to

```
instance Show Location where
	show loc = (title loc) <> sep <> (description loc)
		where sep = replicate (length (title loc)) '-'
```

Having to type in `loc` everytime is tedious. The `RecordWildCards` extension automatically declares variables matching the names of the value's attributes. The result is much more readable

```
{-# LANGUAGE RecordWildCards #-}

instance Show Location where
	show Location{..} = title <> sep <> description
		where sep = replicate (length title) '-'
```

Note that we replaced the `loc` variable name with the type-based `Location{..}` pattern match.

## A Clashing Type

In our game, in each locaton (and in the player's inventory) there will be zero or more items which can be picked up and used. Each item has a title and a description, just like our `Location` type.

We'll create an `Item` type in `src/VForth/Item.hs` with the usual fields and a `TextShow` implementation. This will in fact by identical to the `Location` module except with the name `Location` replaced with `Item`. The type is defined as

```
data Item = Item {
    title :: Text
  , description :: Text
  }
```

Now we add this to our `Location` type:

```
data Location = Location {
    title :: Text
  , description :: Text
  , items :: [Item]
  }
```

Now we need to import `Item`. We could do this using a simple import

```
import VForth.Item
```

However we have an immediate problem, as due to this import there are now clashing declarations and implementation of functions called `title` and `description`.

A simple way to address this is using qualified imports. Replace the `Item` import with

```
import VForth.Item (Item)
import qualified VForth.Item as Item
```

This eliminates the clashes, and now - with the `RecordWildCards` extension - we can amend the `TextShow` implementation to also include the titles of any items in the location:

```
instance TextShow Location where
  showb Location{..} =
    let
      titleLen = fromIntegral (T.length title)
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
      itemText =
        if null items
        then
          ""
        else
          let
            itemTitles = fmap ((<> endl) . Bldr.fromText . Item.title) items
          in
          mconcat (Bldr.fromLazyText "\nIt contains: \n" : itemTitles)
    in
    Bldr.fromText title <> sep <> Bldr.fromText description <> itemText
```

Note that we're using `Item.title` to get the title of the item. 

Ultimately however, there is a better way of handling this. We can prefix the name of the attributes with the type name. Hence `Location` becomes

```
data Location = Location {
    locTitle :: Text
  , locDescription :: Text
  , locItems :: [Item]
  }
```

and `Item` becomes

```
data Item = Item {
    itemTitle :: Text
  , itemDescription :: Text
  }
```

While typing `locTitle loc` is tedious, the `RecordWildCards` extension makes it read much nicer, hence:

```
instance TextShow Location where
  showb Location{..} =
    let
      titleLen = fromIntegral (T.length locTitle)
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
      itemText =
        if null items
        then
          ""
        else
          let
            itemTitles = fmap ((<> endl) . Bldr.fromText . itemTitle) locItems
          in
          mconcat (Bldr.fromLazyText "\nIt contains:\n" : itemTitles)
    in
    Bldr.fromText locTitle 
    <> sep 
    <> Bldr.fromText locDescription 
    <> itemText
```


What's more, it's now possible to export the `Item` module alongside the `Location` module in `src/VForth.hs`:

```
{-# LANGUAGE OverloadedStrings #-}
module VForth (
   module VForth.Location
 , module VForth.Item
 , welcomeMsg
 , isValidUserName
 ) where

import VForth.Location
import VForth.Item

-- etc.
```

## Further Reading: Lenses

A more complex method of accessing type attributes are "lenses". Lenses are particularly useful if you want to chanage a value deep in a hierarchy. They're an attempt to emulate the dot-notation one sees in imperative languages. As an example, consider the following model:

```
data Address = Address {
    addrStreets  :: [Text]
  , addrCity     :: Text
  , addrPostCode :: Text
  , addrRegion   :: Text
  , addrCountry  :: Text
  }
data Company = Company {
    compName :: Text
  , compTel  :: Integer
  , compAddr :: Address
  }
data Person = Person {
    personForenames :: [Text]
  , personSurname   :: Text
  , personAddress   :: Address
  , personEmployer  :: Company
  }
```

Lets say we want to change the city of an individual person's employer. Here's a pure Haskell way of doing it.

```
changeEmployerCity :: Person -> String -> Person
changeEmployerCity p newCity =
  let
    e = personEmployer p
    a = compAddr e
    newAddress = a { addrCity = newCity }
    newComp    = e { compAddr = newAddress }
    newPerson  = p { personEmployer = newComp }
  in
    newPerson
```

Using lenses you can write

```
changeEmployerCity :: Person -> String -> Person
changeEmployerCity p newCity =
	p & personEmployer . compName . addrCity .~ newCity
```

Note that _reading_ the city would be quite different. There's no ampersand and the dot-plus-something operator is at the _start_ of the chain.

```
p ^. personEmployer . compName . addrCity
```

Typically lens implementations are generated automatically using [template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html), Haskell's equivalent of a macro system.
Lenses are slightly controversial. The syntax is rather fiddly, as illustrated by the availability of [cheatsheets](https://github.com/ekmett/lens/wiki/Operators), and some people question the use of imperative style in Haskell. For example, in a language like Java, the following works well:

```
p.getEmployer().getName().setCity(newCity);
```

Because Java applications typically use _references_ to singleton values, this will typically result in the city being effectively changed in all contexts, for all persons employed by that company. Since Haskell passes by value however, we've changed the address for one person while leaving the address of the same company unchanged for all other employed persons. This is arguably a bug, which is much clearer in the long version.

Personally, for the time being, I try to avoid lenses, but since many database and web-frameworks make use of them, it's good to know about them, which is why this [introduction to lenses](http://unbui.lt/#!/post/haskell-another-lens-tutorial), [this short tutorial](http://lens.github.io/tutorial.html) and [this more in-depth one](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial) are all worth reading.
 
Just in passing you should note that the model above uses something called "stringly-typing", where we just use unbounded text types for forenames, post-codes, cities etc. etc. This is an easy way to permit validation bugs, it would be much better to create `Forename`, `Surname`, `PostCode` etc. newtypes wrappers around Text and use smart-constructors to ensure only valid values could be constructed.

## Conclusions

In this brief part of the tutorial we've discussed how to get modules working together without name-clashes. Many approaches exist and are used, my personal prefernce is to prefix attribute names with the type-name, and use the `RecordWildCards` extension to avoid extra typing.

For built-in types, I recommend unqualified imports of types and qualified imports of their associated functions, as I've done perviously with `Text`

```
import Data.Text (Text)
import qualified Data.Text as Text
```

The code for this part of the tutorial is [here](https://github.com/budgefeeney/ventureforth/tree/master/chap5). It also includes amended unit-tests for the `Location` and `Item` types, though it's good to try to code these yourself first.

In the [next part of the tutorial]() we'll address a problem we discovered [previously](fixme/day3): the fact that one can create invalid values for Location and now Item. We'll look into hiding constructors, the use of validating smart-constructors and view-types. We'll also cover regular expressions in Haskell.
