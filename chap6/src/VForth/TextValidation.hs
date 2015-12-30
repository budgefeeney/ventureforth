{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : VForth.GameText
Description : Validated classes of text
Copyright   : (c) Bryan Feeney 2015

Provides a simple generic way of specifying an applying a validation
strategy to text. Returns on first failure, rather than continuing
to the end.
-}
module VForth.TextValidation (
    ValidationConstraints (..)
  , validate
  ) where

import Data.Maybe (fromJust)

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.ICU hiding (compare)
import Data.Monoid ((<>))
import Data.Default

import TextShow hiding (fromText)
import VForth.Errors


{-
  The "rules" governing how to validate a piece of text. Simply put this is
  just the minimum and maximum lengths, the list of valid characters, a label
  to use in generating error messages, and two functions called before and
  after all other checks, which may do additional checks and / or some
  pre-processing and post-processing
-}
data ValidationConstraints = ValidationConstraints {
  -- | A description of this type of text, used in error messages
    vconsTextLabel :: Text
  -- | Minimum permitted length, zero by default
  , vconsMinLength :: Int
  -- | Maximum permitted lenght, @max :: Int@ by default
  , vconsMaxLength :: Int
  -- | The list of characters allowed in the string. If nothing, all characters
  --   are allowed. Nothing by default. If case is not important, specify only
  --   lower-case characters here, and set @vconsCaseSensitive@ to @false@
  , vconsValidChars :: Maybe String
  -- | Whether we should ignore case when looking at which characters are valid.
  --   True by default
  , vconsIgnoreCase :: Bool
  -- | Called before the text is validated. If a @Left@ value is returned no
  --   more validation checks take place (usual @>>=@ behaviour). By default
  --   this strips the string
  , vconsBeforeValidation :: Text -> Either Error Text
  -- | Called after all other validation checks have been executed, if and only
  --   if they've all succeeded. By default this does nothing
  , vconsAfterValidation :: Text -> Either Error Text
}

instance Default ValidationConstraints where
  def = ValidationConstraints {
      vconsTextLabel = "text"
    , vconsMinLength = 0
    , vconsMaxLength = maxBound
    , vconsValidChars = Nothing -- Paradoxically means everything is allowed
    , vconsIgnoreCase = True
    , vconsBeforeValidation = Right . Text.strip
    , vconsAfterValidation  = Right
    }

-- | Creates a regular expression that will match all characters not in the
--   given list of valid characters.
invalidCharsRe :: String  -- ^ the list of _valid_ characters
               -> Bool    -- ^ set true for case-insensitive, false otherwise
               -> Regex
invalidCharsRe validChars ignoreCase =
  let
    escapableChars = ['(', ')', '[', ']', '\\', '-']
    escapeBrackets c acc =
      if c `elem` escapableChars then '\\' : c : acc else c : acc
    escValidChrs = foldr escapeBrackets "" validChars
  in
    regex [CaseInsensitive | ignoreCase] . Text.pack $ "[^" <> escValidChrs <> "]"

-- | Performs all checks on a piece of text (min-length, max-length, valid
--   characters, and any @extraValidation@) if necessary. If all checks pass,
--   it is
validate :: ValidationConstraints -- ^ Specifies how to validate the text
          -> Text                 -- ^ The text to validate and covert
          -> Either Error Text
validate ValidationConstraints{..}  rawText =
  let
    checkEmpty t =
      if vconsMinLength > 0 && Text.null t then
        Left . InvalidGameText $ vconsTextLabel <> " which is empty"
      else
        Right t

    checkMinLen t =
      if Text.length t < vconsMinLength
      then
        Left . InvalidGameText $ vconsTextLabel
          <> " whose length of " <> showt (Text.length t)
          <> " characters is below the minimum of "
          <> showt vconsMinLength <> " characters"
      else
        Right t

    checkMaxLen t =
      if Text.length t > vconsMaxLength
      then
        Left . InvalidGameText $ vconsTextLabel
          <> " whose length of " <> showt (Text.length t)
          <> " characters is over the maximum of "
          <> showt vconsMaxLength <> " characters"
      else
        Right t

    checkInvalidChars t =
      let
        invalidRegex   = invalidCharsRe <$> vconsValidChars <*> Just vconsIgnoreCase
        invalidMatches = maybe [] (`findAll` t) invalidRegex
      in
        if (not . null) invalidMatches
        then
          Left . InvalidGameText $ vconsTextLabel
            <> " which contains the invalid character sequences: "
            <> Text.intercalate "; " (map (fromJust . group 0) invalidMatches)
        else
          Right t
  in
    vconsBeforeValidation rawText
    >>= checkEmpty
    >>= checkMinLen
    >>= checkMaxLen
    >>= checkInvalidChars
    >>= vconsAfterValidation
