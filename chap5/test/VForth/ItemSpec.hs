{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.ItemSpec where

import qualified Data.Text as T
import Data.Text.Arbitrary
import TextShow
import Data.Char(isSpace)
import Test.Hspec
import Data.Monoid ((<>))
import Test.QuickCheck
import VForth

newtype TestableItem = TestableItem Item
instance Arbitrary TestableItem where
  arbitrary = TestableItem <$> (
    newItem <$> arbitrary <*> arbitrary
    )
instance Show TestableItem where
  show (TestableItem Item{..}) =
    "Item { title=\"" <> show itemTitle <> "\", description=\"" <> show itemDescription <> "\" }"

newItem :: Text -> Text -> Item
newItem titleText descText = Item {
    itemTitle = titleText
  , itemDescription = descText
  }

spec :: Spec
spec = do
  describe "Item Display" $ do
    it "Show puts title before dashes before a description" $ do
      showt (newItem "My Title" "The complete description.") `shouldBe` "My Title: The complete description."

    it "Title should be included in showable output" $ property $
      \(TestableItem i@Item{..}) -> itemTitle `T.isInfixOf` showt i

    it "Description should be included in showable output" $ property $
      \(TestableItem i@Item{..}) -> itemDescription `T.isInfixOf` showt i

    it "Showable output is never blank"  $ property $
      \(TestableItem i@Item{..}) ->
        (not . T.null $ itemTitle) && (not . T.null $ itemDescription)
        ==> any (not . isSpace) (T.unpack (showt i))
