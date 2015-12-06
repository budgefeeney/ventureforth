{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|
 Module      : VForth
 Description : An item a player can pick up and use
 Copyright   : (c) Bryan Feeney 2015

 Representation of items that can be picked up and used by the player, and
 all associated functions
-}
module VForth.Item (
    Item(..)
) where

import Data.Monoid ((<>))
import TextShow

import VForth.GameText

{-|
  An item that can be picked up and used by a player
-}
data Item = Item {
    itemTitle :: Title
  , itemDescription :: Description
  }

instance TextShow Item where
  showb Item{..} =
    showb itemTitle <> ": " <> showb itemDescription
