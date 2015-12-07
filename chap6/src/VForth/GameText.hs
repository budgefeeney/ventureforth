{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : VForth.GameText
Description : Validated classes of text
Copyright   : (c) Bryan Feeney 2015

Wraps raw unvalidated text into ADTs which can only be constructed using
valid text.
-}
module VForth.GameText (
  -- * Titles & Descriptions of items and locations
    SafeTextConstraints
  , Title
  , title
  , titleConstraints
  , Description
  , description
  , descriptionConstraints
) where

import Data.Either (isRight)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Text.ICU hiding (compare)
import Data.Monoid ((<>))
import Data.Default

import TextShow hiding (fromText)
import VForth.Errors

{-
  The "rules" governing how to validate a piece of text. As well as the usual
  minimum and maximum lengths, whether to trim leading and training whitespace,
  and the list of valid characters, you can also provide functions to be run
  before and after all other checks have passed in case there's some additional
  work you'd like done
-}
data SafeTextConstraints = SafeTextConstraints {
  -- | A description of this type of safe text, used in errors
    textLabel :: Text
  -- | Minimum permitted length, zero by default
  , minLength :: Int
  -- | Maximum permitted lenght, @max :: Int@ by default
  , maxLength :: Int
  -- | The list of characters allowed in the string. If nothing, all characters
  --   are allowed. Nothing by default
  , validChars :: Maybe String
  -- | Called before the text is validated. If a @Left@ value is returned no
  --   more validation checks take place (usual @>>=@ behaviour). By default
  --   this strips the string
  , beforeValidation :: Text -> Either Error Text
  -- | Called after all other validation checks have been executed, if and only
  --   if they've all succeeded. By default this does nothing
  , afterValidation :: Text -> Either Error Text
}

instance Default SafeTextConstraints where
  def = SafeTextConstraints {
      textLabel = "text"
    , minLength = 0
    , maxLength = maxBound
    , validChars = Nothing -- Paradoxically means everything is allowed
    , beforeValidation = Right . Text.strip
    , afterValidation  = Right
    }

-- | Creates a regular expression that will match all characters not in the
--   given list of valid characters. This regex is _not_ case-sensitive.
invalidCharsRe :: String       -- ^ the list of _valid_ characters
               -> Regex
invalidCharsRe validChars =
  regex [CaseInsensitive] . Text.pack $ "[^" <> validChars <> "]"

-- | Performs all checks on a piece of text (min-length, max-length, valid
--   characters, and any @extraValidation@) if necessary. If all checks pass,
--   it is
validate :: SafeTextConstraints  -- ^ Specifies how to validate the text
          -> Text                -- ^ The text to validate and covert
          -> Either Error Text
validate SafeTextConstraints{..}  rawText =
  let
    checkEmpty t =
      if minLength > 0 && Text.null t then
        Left . InvalidGameText $ textLabel <> " which is empty"
      else
        Right t

    checkMinLen t =
      if Text.length t < minLength
      then
        Left . InvalidGameText $ textLabel
          <> " whose length of " <> showt (Text.length t)
          <> " characters is below the minimum of "
          <> showt minLength <> " characters"
      else
        Right t

    checkMaxLen t =
      if Text.length t > maxLength
      then
        Left . InvalidGameText $ textLabel
          <> " whose length of " <> showt (Text.length t)
          <> " characters is over the maximum of "
          <> showt maxLength <> " characters"
      else
        Right t

    checkInvalidChars t =
      let
        foundInvalidChars = case validChars of
          (Just chars) -> (not . null) $ findAll (invalidCharsRe chars) t
          Nothing      -> False

        -- printableInvalidCharMatches =
        --   Text.intercalate "; " (map (fromJust . group 0) <$> invalidCharMatches)
      in
        if foundInvalidChars
        then
          Left . InvalidGameText $ textLabel
            <> " which contains the invalid character sequences: "
            -- <> printableInvalidCharMatches
        else
          Right t
  in
    beforeValidation rawText
    >>= checkEmpty
    >>= checkMinLen
    >>= checkMaxLen
    >>= checkInvalidChars
    >>= afterValidation


-- | The title of an item or location
newtype Title = Title Text deriving (Eq, Ord)

-- | The constraints specifying which text values are valid titles
titleConstraints :: SafeTextConstraints
titleConstraints = def {
    textLabel = "title"
  , minLength = 3
  , maxLength = 30
  , validChars = Just $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ",.:;£$-!?&()' "
  }

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


-- | A Description of an item or location
newtype Description = Description Text deriving (Eq, Ord)

-- | The constraints specifying which text values are valid descriptions
descriptionConstraints :: SafeTextConstraints
descriptionConstraints = def {
    textLabel = "description"
  , minLength = 20
  , maxLength = 500
  , validChars = Just $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ",.:;£$-!?&()' \r\n\t"
  }

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
