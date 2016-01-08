{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.GameTextSpec (
    TestableTitle(..)
  , TestableDescription(..)
  , unsafeRight
  , spec
) where

import Test.Hspec
import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.Monoid ((<>))
import Data.Text.Arbitrary ()
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad

import VForth.GameText
import Data.Text.Validation


unsafeRight :: Show l => Either l r -> r
unsafeRight (Right r) = r
unsafeRight (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

-- | Creates a generator for wrapped text-types that fulfills its constraints
arbitraryFromConstraints
  :: ValidationConstraints -- ^ Provide minimum and maximum length, and regex
  -> Maybe String          -- ^ Provides the list of valid chars. All are valid if Nothing
  -> (Text -> a)           -- ^ A function to wrap the generated text in a newtype
  -> Gen a
arbitraryFromConstraints ValidationConstraints{..} validChars makeFn =
  let
    validCharsGen   = maybe arbitrary elements validChars
    validSubSeqGen  = choose (vconsMinLength, vconsMaxLength)
                       >>= \count -> replicateM count validCharsGen
    validTextGen    = Text.pack <$> validSubSeqGen
  in
    makeFn <$> validTextGen

newtype TestableTitle = TestableTitle Title deriving Show
instance Arbitrary TestableTitle where
  arbitrary =
    arbitraryFromConstraints
      titleConstraints
      (Just titleChars)
      (TestableTitle . unsafeRight . title)

newtype TestableDescription = TestableDescription Description deriving Show
instance Arbitrary TestableDescription where
  arbitrary =
    arbitraryFromConstraints
      descriptionConstraints
      (Just descriptionChars)
      (TestableDescription . unsafeRight . description)

spec :: Spec
spec = return ()
