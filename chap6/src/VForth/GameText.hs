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
  , titleText
  , Description
  , description
  , descText
) where

import Data.Either (isRight)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Text.ICU
import Data.Monoid ((<>))
import Data.Maybe

import TextShow
import VForth.Errors


delimMatches :: Text -> [Match] -> Text
delimMatches delim matches =
  let
    mtexts = map (fromJust . group 0) matches
  in
    Text.intercalate delim mtexts

-- | The title of an item or location
newtype Title = Title { titleText :: Text } deriving Show

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
title =
  let
    badChars = regex [CaseInsensitive] "[^a-z0-9 ,.:;£$\\-!?&()']+"
  in
    validateText 3 30 badChars Title "short-text"

instance TextShow Title where
  showb = Bldr.fromText . titleText


-- | A description of an item or location
newtype Description = Description { descText :: Text } deriving Show

{-|
  Validates the given text and if valid, converts to a Description value.
  A valid value is between 20 and 500 characters in length, and contains only
  letters, digits, spaces and normal punctuation (e.g. ampersands are allowed,
  forward slashes are note). It can span multiple lines, so newlines are also
  acceptable. For example this validates

  >>> (isRight . description . Text.pack) "This is a\nmultiline description"
  True

  But these do not:

  >>> map (isRight . description . Text.pack) [ "", " ", "H", "Mungo", "C:\\Windows\\System\\MyFile.txt", (replicate 600 'A') ]
  [False,False,False,False,False,False]
-}
description :: Text -> Either Error Description
description =
  let
    badChars = regex [CaseInsensitive] "[^a-z0-9 ,.:;£$\\-!?&()\n\t\"']+"
  in
    validateText 20 500 badChars Description "medium-text"

instance TextShow Description where
  showb = Bldr.fromText . descText


-- | Validates the given text according to the given criteria, and then
--   wraps it up in the appropriate type. Empty text is assumed to be
--   immediately invalid.
validateText :: Int         -- ^ The minimum length (inclusive)
             -> Int         -- ^ The maximum length (exclusive)
             -> Regex       -- ^ A regular expression matching _invalid_ characters
             -> (Text -> a) -- ^ A function converting the text to the output type
             -> Text        -- ^ A descriptive label for the text used in error messages
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
      Left . InvalidGameText $ textDesc <> " whose length of " <> showt (Text.length t) <> " characters is below the minimum of " <> showt minLen <> " characters"
    else if Text.length t > maxLen
    then
      Left . InvalidGameText $ textDesc <> " whose length of " <> showt (Text.length t) <> " characters is over the maximum of " <> showt maxLen <> " characters"
    else
      let
        badChars = findAll badCharsRe t
      in
        if (not . null) badChars
        then
          Left . InvalidGameText $ textDesc <> " which contains the invalid character sequences: " <> delimMatches "; " badChars
        else
          Right . constructorFunc $ t
