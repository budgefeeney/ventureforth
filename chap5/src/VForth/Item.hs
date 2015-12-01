module VForth.Item (
  Item(..)
) where

import Data.Text(Text)
import Data.Text.Lazy.Builder as Bldr
import TextShow

{-|
  An item that can be picked up and used by a player
-}
data Item = Item {
    itemTitle :: Text
  , itemDescription :: Text
  }

instance TextShow Item where
  showb = Bldr.fromText . itemTitle
