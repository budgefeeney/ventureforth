{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
 Module      : VForth
 Description : Locations in the game world the player can visit.
 Copyright   : (c) Bryan Feeney 2015

Representation of locations that the player can move to in the game, and
all associated functions
-}module VForth.Location (
   Location(..)
 ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Monoid ((<>))
import TextShow

import VForth.Item

data Location = Location {
    locTitle :: Text
  , locDescription :: Text
  , locItems :: [Item]
  }

instance TextShow Location where
  showb Location{..} =
    let
      titleLen = fromIntegral (T.length locTitle)
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
      itemText =
        if null locItems
        then
          ""
        else
          let
            itemTitles = fmap ((<> endl) . Bldr.fromText . itemTitle) locItems
          in
          mconcat (Bldr.fromLazyText "\nIt contains:\n" : itemTitles)
    in
    Bldr.fromText locTitle <> sep <> Bldr.fromText locDescription <> itemText
