{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

Module      : VForth.GameText
Description : Validated classes of text
Copyright   : (c) Bryan Feeney 2015

The full list of all possible errors that may occur within the VForth
application
-}
module VForth.Errors (
  Error(..),
  errorDescription
) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Monoid ((<>))
import TextShow

{-|
 The list of possible application errors
-}
data Error =
  InvalidGameText { msg :: Text }

instance TextShow Error where
  showb = Bldr.fromText . errorDescription

instance Show Error where
  show = Text.unpack . errorDescription

-- | Converts an @Error@ value to text suitable for display to the user
errorDescription :: Error -> Text
errorDescription InvalidGameText{..} =
  "The game data is corrupted: there is a " <> msg
