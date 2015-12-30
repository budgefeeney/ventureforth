{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Address (
  Address,
  Address
  HouseIdentifier,
  CountryCode
) where

import Data.Text (Text)
import Data.Text.ICU
import Data.Maybe (isNothing)

newtype StreetName = StreetName Text deriving Show
newtype CityName   = CityName Text deriving Show

type PostCode = Text
type CountryName = Text

data CountryCode =
    UnitedKingdom PostCode
  | Ireland (Maybe PostCode)
  | France PostCode
  | UnknownCountry CountryName (Maybe PostCode)

data HouseIdentifier = HouseIdentifier {
    houseName :: Text
  , houseIdNum :: Int
  , houseIdNumQualifier :: Text
  , flatName :: Text
  , flatIdNum :: Int
  , flatIdNumQualifier :: Text
  }

data Address = Address {
	addrHouseId :: HouseIdentifier
  , addrStreets :: [StreetName]
  , addrCity :: CityName
  , addrCountryCode :: CountryCode
  }

type MyError = Text

streetName :: Text -> Either MyError StreetName
streetName t =
  let
   (minLen, maxLen) = (3, 30)
   validChars = regex [CaseInsensitive] "^(?:\\s*\\p{digit}*p{IsAlphabetic}+\\b){,10}\\s*$"
   input = Text.strip t
   inputLen = Text.length input

   checkNotEmpty =
     if inputLen == 0
       then Left "Cannot have a blank street-name"
       else Right t

    checkLengthRange =
      if inputLen < titleMin || inputLen > titleMin
        then Left "The street-name length (" <> showt inputLen <> ") is outside the acceptable range of (" <> showt minLen <> "," <> showt maxLen <> ")"
        else Right t

    checkChars =
      if (isNothing . find) input
        then Left "The street-name contains more than 10 words, or invalid characters"
        else Right t
  in
    checkNotEmpty >>= checkLengthRange >>= checkChars
