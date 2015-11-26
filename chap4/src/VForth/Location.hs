{-# LANGUAGE OverloadedStrings #-}
module VForth.Location (
   Location(..)
 ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as Bldr
import Data.Monoid ((<>))
import TextShow

data Location = Location {
    title :: Text
  , description :: Text
  }

instance TextShow Location where
  showb l =
    let
      titleLen = fromIntegral (T.length (title l))
      dashes   = Bldr.fromLazyText $ LT.replicate titleLen (LT.pack "-")
      endl     = Bldr.singleton '\n'
      sep      = endl <> dashes <> endl
    in
    Bldr.fromText (title l) <> sep <> Bldr.fromText (description l)
