{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.LocationSpec where

import Data.Text.Arbitrary
import TextShow
import Test.Hspec
import Data.Monoid ((<>))
import Test.QuickCheck
import VForth

import VForth.GameTextSpec
import VForth.ItemSpec

newtype TestableLocation = TestableLocation Location
instance Arbitrary TestableLocation where
  arbitrary = newLocation <$> arbitrary <*> arbitrary <*> arbitrary

instance Show TestableLocation where
  show (TestableLocation Location{..}) =
    "Location { title=\"" <> show locTitle <> "\", description=\"" <> show locDescription <> "\" }"


-- | Creates a new Location value for testing
newLocation :: TestableTitle -> TestableDescription -> [TestableItem] -> TestableLocation
newLocation (TestableTitle t) (TestableDescription d) is =
  let
    plainItems = map (\(TestableItem i) -> i) is
  in
    TestableLocation Location {
        locTitle = t
      , locDescription = d
      , locItems = plainItems
      }

-- | Creates a new Location value on the assumption that the given title and
--   descriptions are valid. The test-suite will crash out if they're not.
unsafeNewLocation :: Text -> Text -> Location
unsafeNewLocation tText dText =
  let
    t = unsafeRight . title $ tText
    d = unsafeRight . description $ dText
  in
    Location {
       locTitle = t
     , locDescription = d
     , locItems = []
     }


spec :: Spec
spec = do
  describe "Location Display" $ do
    it "Show puts title before dashes before a description" $ do
      showt (unsafeNewLocation "My Title" "The complete description.") `shouldBe` "My Title\n--------\nThe complete description."

    -- it "Title should be included in showable output" $ property $
    --   \(TestableLocation l@Location{..}) -> titleText locTitle `T.isInfixOf` showt l
    --
    -- it "Description should be included in showable output" $ property $
    --   \(TestableLocation l@Location{..}) -> descText locDescription `T.isInfixOf` showt l
    --
    -- it "Showable output is never blank"  $ property $
    --   \(TestableLocation l@Location{..}) ->
    --     any (not . isSpace) (T.unpack (showt l))
    --
    -- it "Location with items has item titles" $ property $
    --   \(TestableLocation l@Location{..}) ->
    --     let
    --       output      = showt l
    --       itemTitles  = map (titleText . itemTitle) locItems
    --       titlesFound = map (flip T.isInfixOf output) itemTitles
    --     in
    --       foldl (&&) True titlesFound
