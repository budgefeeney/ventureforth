{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : VForth.GameText
Description : Validated classes of text
Copyright   : (c) Bryan Feeney 2015

Wraps raw unvalidated text into ADTs which can only be constructed using
valid text.
-}
module VForth.GameText (
  -- * Titles & Descriptions of items and locations
    Title
  , title
  , Description
  , description
  -- * Non-empty, text-fragments suitable for display to the user
  , ShortDisplayText
  , shortDisplayText
  , MedDisplayText
  , medDisplayText
) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU
import Data.Monoid ((<>))
import Data.Maybe
import Data.Either

import VForth.Errors

-- | A regular expression given the acceptable characters for short and long text
badCharsRe :: Regex
badCharsRe = regex [CaseInsensitive] "[^a-z0-9 ,.:;£$\\-!?&()]+"

delimMatches :: Text -> [Match] -> Text
delimMatches delim matches =
  let
    mtexts = map (fromJust . group 0) matches
  in
    Text.intercalate delim mtexts

-- | A short piece of non-empty text suitable to display to the user.
newtype ShortDisplayText = ShortDisplayText { shortToText :: Text } deriving Show

{-|
  Validates the given text and if valid, converts to a ShortDisplayText value.
  A valid value is less than 30 characters in length, and contain only letters,
  digits, and space characters. For example this validates

  >>> (isRight . shortDisplayText . Text.pack) "My title"
  True

  But these do not:

  >>> map (isRight. shortDisplayText . Text.pack) [ "", " ", "He\tllo", (replicate 50 'A') ]
  [False, False, False, False]

  However as we trim the text before validating, the following is fine, despite
  the prohibition on whitepace other than plain spaces.

  >>> (isRight . shortDisplayText . Text.pack) "\tHello\r\n"
  True

-}
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
        badChars = findAll badCharsRe t
      in
        if (not . null) badChars
        then
          Left (InvalidGameText $ "short-text which contains the invalid character sequences [" <> delimMatches ", " badChars <> "]" )
        else
          Right . ShortDisplayText $ t

-- | The title of a location, item or other in-game entity
type Title = ShortDisplayText

{-|
  Validates the given text and if valid, converts to a Title value, the
  same as 'shortDisplayText'
-}
title :: Text -> Either Error Title
title = shortDisplayText


-- | A medium-length piece of non-empty text suitable to display to the user
newtype MedDisplayText = MedDisplayText { medToText :: Text } deriving Show

{-| Validates the given text and if valid, converts to a LongDisplayText value -}
medDisplayText :: Text -> Either Error MedDisplayText
medDisplayText t = Right. MedDisplayText $ t

-- | A description of a location, item or other in-game entity
type Description = MedDisplayText

{-|
  Validates the given text and if valid, converts to a Description value, the
  same as 'longDisplayText'
-}
description :: Text -> Either Error Description
description = medDisplayText
