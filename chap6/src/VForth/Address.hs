{-# LANGUAGE OverloadedStrings #-}
module Address (
  Address
) where

import Data.Text (Text)
--import qualified Data.Text as Text

newtype AddressFragment = AddressFragment Text deriving Show
type HouseName  = AddressFragment
type StreetName = AddressFragment
type CityName   = AddressFragment

data Country =
    UnitedKingdom
  | Ireland
  | France
  | UnknownCountry AddressFragment
  deriving Show

newtype PostCode = PostCode { asText :: Text } deriving Show

data Address = Address {
    addrName :: HouseName
  , addrNumber :: Int
  , addrStreets :: [StreetName]
  , addrCity :: CityName
  , addrCountry :: Country
  , addrPostCode :: Maybe PostCode
  } deriving Show
