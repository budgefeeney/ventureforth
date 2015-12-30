{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module VForth.GameTextArb (
    TestableTitle(..)
  , TestableDescription(..)
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
import Data.Maybe
import qualified Data.Char as Char

import VForth.GameText
import VForth.TextValidation


unsafeRight :: Show l => Either l r -> r
unsafeRight (Right r) = r
unsafeRight (Left  l) = error ("Unexpectedly encountered left-value " <> show l)

arbitraryFromConstraints :: ValidationConstraints -> (Text -> a) -> Gen a
arbitraryFromConstraints ValidationConstraints{..} makeFn =
  let
    validTextGen =
      if isNothing vconsValidChars
      then
        arbitrary :: Gen Text
      else
        let
          -- Generate a random string of characters
          validCharsGen   = elements . fromJust $ vconsValidChars :: Gen Char
          validSubSeqGen  = choose (vconsMinLength, vconsMaxLength)
                             >>= \count -> replicateM count validCharsGen

          -- Switch cases if the characters are all lower
          upcase doit s = if doit then Char.toUpper <$> s else s

          upCaseChoice = if vconsIgnoreCase then [False, True] else [False, False]
          doUpCase = elements upCaseChoice
          validStringGen = upcase <$> doUpCase <*> validSubSeqGen
        in
          Text.pack <$> validStringGen
  in
    makeFn <$> validTextGen

newtype TestableTitle = TestableTitle Title deriving Show
instance Arbitrary TestableTitle where
  arbitrary =
    arbitraryFromConstraints
      titleConstraints
      (TestableTitle . unsafeRight . title)

newtype TestableDescription = TestableDescription Description deriving Show
instance Arbitrary TestableDescription where
  arbitrary =
    arbitraryFromConstraints
      descriptionConstraints
      (TestableDescription . unsafeRight . description)

spec :: Spec
spec = return ()
