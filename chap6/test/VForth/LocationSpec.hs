{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.LocationSpec where

import qualified Data.Text as T
import Data.Text.Arbitrary
import TextShow
import Data.Char(isSpace)
import Test.Hspec
import Data.Monoid ((<>))
import Test.QuickCheck
import VForth

newtype TestableLocation = TestableLocation Location
instance Arbitrary TestableLocation where
  arbitrary = TestableLocation <$> (
    newLocation <$> arbitrary <*> arbitrary
    )
instance Show TestableLocation where
  show (TestableLocation Location{..}) =
    "Location { title=\"" <> show locTitle <> "\", description=\"" <> show locDescription <> "\" }"

newLocation :: Text -> Text -> Location
newLocation titleText descText = Location {
    locTitle = titleText
  , locDescription = descText
  , locItems = []
  }

spec :: Spec
spec = do
  describe "Location Display" $ do
    it "Show puts title before dashes before a description" $ do
      showt (newLocation "My Title" "The complete description.") `shouldBe` "My Title\n--------\nThe complete description."

    it "Title should be included in showable output" $ property $
      \(TestableLocation l@Location{..}) -> locTitle `T.isInfixOf` showt l

    it "Description should be included in showable output" $ property $
      \(TestableLocation l@Location{..}) -> locDescription `T.isInfixOf` showt l

    it "Showable output is never blank"  $ property $
      \(TestableLocation l@Location{..}) ->
        (not . T.null $ locTitle) && (not . T.null $ locDescription)
        ==> any (not . isSpace) (T.unpack (showt l))

    it "Location with items has item titles" $ do
      let item1 = Item {
          itemTitle = "A wad of of cash"
        , itemDescription = "It's a wad of cash. It's probably yours."
        }
      let item2 = Item {
          itemTitle = "A pile of laundry"
        , itemDescription =
            "Once these were your clothes. Now they're a hideous biological "
            <> "experiment. You should really do something about them."
        }
      let loc = Location {
          locTitle = "Your bedroom"
        , locDescription =
            "You're in your bedroom. It's an utterly disgusting tip of a "
            <> "place. Dirty coffee mugs everywhere, bits of computer and "
            <> "motorbike all over the floor. It's an outrage. You can leave "
            <> "by going north, and maybe you should."
        , locItems = [ item1, item2 ]
        }
      let expectedText = "Your bedroom\n"
           <> "------------\n"
           <> "You're in your bedroom. It's an utterly disgusting tip of a "
           <> "place. Dirty coffee mugs everywhere, bits of computer and "
           <> "motorbike all over the floor. It's an outrage. You can leave "
           <> "by going north, and maybe you should.\n"
           <> "It contains:\n"
           <> "A wad of of cash\n"
           <> "A pile of laundry\n" -- bit of a bug, an extra newline for items
      showt loc `shouldBe` expectedText
