{-# OPTIONS_GHC -fno-warn-unused-imports #-} -- to enable the doc-test only import of Data.Either
{-# LANGUAGE OverloadedStrings #-}




{-|
Module      : VForth.GameText
Description : Validated classes of text
Copyright   : (c) Bryan Feeney 2015

Defines the validating text-wrappers
-}
module VForth.GameText (
  -- * Titles & Descriptions of items and locations
    Title
  , title
  , titleChars
  , titleConstraints
  , Description
  , description
  , descriptionChars
  , descriptionConstraints
) where



import Data.Char
import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Text.ICU
import Data.Monoid ((<>))
import Data.Default

import TextShow hiding (fromText)
import VForth.Errors
import Data.Text.Validation
import Data.Either (isRight)


-- | The title of an item or location
newtype Title = Title Text deriving (Eq, Ord)

-- | Character classes accepted by the title validation regex
titleRegexClasses :: Text
titleRegexClasses = "\\p{Alphabetic}\\p{Digit}\\p{Punct}+\\-<=>/$£€"

-- | Constrains what kind of titles are accepted
titleConstraints :: ValidationConstraints
titleConstraints = def {
    vconsTextLabel = "title"
  , vconsMinLength = 3
  , vconsMaxLength = 30
  , vconsInvalidRegex =
      Just $ regex [CaseInsensitive] ("[^" <> titleRegexClasses <> " &&[^\\\\]]")
  }

-- | Sample characters that can be used to construct arbitrary instances
titleChars :: [Char]
titleChars =
  ['A'..'Z']
    <> ['a'..'z']
    <> ['0'..'9']
    <> "!\"#$%&'()+,-./:;<=>?@[]_{}"
    <> " "
    <> (chr <$> ([0x00C0..0x01B7] \\ [0x00D7, 0x00F7]))


{-|
  Validates the given text and if valid, converts to a Title value.
  A valid value is between 3 and 30 characters in length, and contains only
   letters, digits, spaces and normal punctuation (e.g. ampersands are allowed,
   forward slashes are note). For example this validates

  >>> (isRight . title . Text.pack) "My title"
  True

  But these do not:

  >>> map (isRight . title . Text.pack) [ "", " ", "H", "Mungo\\", "He\tllo", (replicate 50 'A') ]
  [False,False,False,False,False,False]

  However as we trim the text before validating, the following is fine, despite
  the prohibition on whitepace other than plain spaces.

  >>> (isRight . title . Text.pack) "\tHello\r\n"
  True

-}
title :: Text -> Either Error Title
title text = Title <$> validate titleConstraints text

instance TextShow Title where
  showb (Title t) = Bldr.fromText t

instance Show Title where
  show (Title t) = Text.unpack t


-- | The description of an item or location
newtype Description = Description Text deriving (Eq, Ord)


-- | The constraints specifying which text values are valid descriptions
descriptionConstraints :: ValidationConstraints
descriptionConstraints = def {
    vconsTextLabel = "description"
  , vconsMinLength = 20
  , vconsMaxLength = 500
  , vconsInvalidRegex =
      Just $ regex [CaseInsensitive] ("[^" <> titleRegexClasses <> "\\p{Space}&&[^\\\\]]")
  }

-- | Sample characters that can be used to construct arbitrary instances
descriptionChars :: [Char]
descriptionChars =
  titleChars <> "\n\r\f\t" -- basically we just allow newlines

{-|
  Validates the given text and if valid, converts to a Description value.
  A valid value is between 20 and 500 characters in length, and contains only
  letters, digits, spaces and normal punctuation (e.g. ampersands are allowed,
  forward slashes are not). It can span multiple lines, so newlines are also
  acceptable. For example this validates

  >>> (isRight . description . Text.pack) "This is a\nmultiline description"
  True

  But these do not:

  >>> map (isRight . description . Text.pack) [ "", " ", "H", "Mungo", "C:\\Windows\\System\\MyFile.txt", (replicate 600 'A') ]
  [False,False,False,False,False,False]
-}
description :: Text -> Either Error Description
description text = Description <$> validate descriptionConstraints text

instance TextShow Description where
  showb (Description d) = Bldr.fromText d

instance Show Description where
  show (Description d) = Text.unpack d
