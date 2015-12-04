{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.Item (
  Item(..)
) where

import Data.Text(Text)
import Data.Text.Lazy.Builder as Bldr
import Data.Monoid ((<>))
import TextShow

{-|
  An item that can be picked up and used by a player
-}
data Item = Item {
    itemTitle :: Text
  , itemDescription :: Text
  }

instance TextShow Item where
  showb Item{..} =
    let
      title = Bldr.fromText itemTitle
      desc  = Bldr.fromText itemDescription
      sep   = Bldr.fromText ": "
    in
      title <> sep <> desc
