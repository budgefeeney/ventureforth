{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
 Module      : VForth
 Description : Locations in the game world the player can visit.
 Copyright   : (c) Bryan Feeney 2015

Representation of locations that the player can move to in the game, and
all associated functions
-}
module VForth.Location (
   Location(..)
 , location
 ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Monoid ((<>))
import TextShow

import VForth.Item
import VForth.GameText

data Location = Location {
    locTitle :: Title
  , locDescription :: Description
  , locItems :: [Item]
  }

-- | Creates a new location value with the given title and description
location :: Title -> Description -> [Item] -> Location
location t d i =
  Location {
    locTitle = t
  , locDescription = d
  , locItems = i
  }

instance TextShow Location where
  showb Location{..} =
    let
      titleLen = fromIntegral . T.length . showt $ locTitle
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
      itemText =
        if null locItems
        then
          ""
        else
          let
            itemTitles = fmap ((<> endl) . showb . itemTitle) locItems
          in
          mconcat (Bldr.fromLazyText "\nIt contains:\n" : itemTitles)
    in
    showb locTitle <> sep <> showb locDescription <> itemText
