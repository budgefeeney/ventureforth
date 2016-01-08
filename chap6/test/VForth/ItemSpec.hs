{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.ItemSpec where

import qualified Data.Text as T
import Data.Text.Arbitrary
import TextShow
import Data.Char (isSpace)
import Test.Hspec
import Data.Monoid ((<>))
import Test.QuickCheck
import VForth

import VForth.GameTextSpec


newtype TestableItem = TestableItem Item
instance Arbitrary TestableItem where
  arbitrary = newItem <$> arbitrary <*> arbitrary

instance Show TestableItem where
  show (TestableItem Item{..}) =
    "Item { title=\"" <> show itemTitle <> "\", description=\"" <> show itemDescription <> "\" }"


-- | Create a new item
newItem :: TestableTitle -> TestableDescription -> TestableItem
newItem (TestableTitle t) (TestableDescription d) =
  TestableItem Item {
     itemTitle = t
   , itemDescription = d
   }


-- | Creates a new item on the assumption the given titles and descriptipns are
--   valid. The test-spec will crash if they're not.
unsafeNewItem :: Text -> Text -> Item
unsafeNewItem tText dText =
  let
    t = unsafeRight . title $ tText
    d = unsafeRight . description $ dText
  in
    Item {
       itemTitle = t
     , itemDescription = d
     }


spec :: Spec
spec = do
  describe "Item Display" $ do
    it "Show puts title before dashes before a description" $ do
      showt (unsafeNewItem "My Title" "The complete description.") `shouldBe` "My Title: The complete description."

    it "Title should be included in showable output" $ property $
      \(TestableItem i@Item{..}) -> showt itemTitle `T.isInfixOf` showt i

    it "Description should be included in showable output" $ property $
      \(TestableItem i@Item{..}) -> showt itemDescription `T.isInfixOf` showt i

    it "Showable output is never blank"  $ property $
      \(TestableItem i@Item{..}) ->
        any (not . (isSpace)) (T.unpack (showt i))
