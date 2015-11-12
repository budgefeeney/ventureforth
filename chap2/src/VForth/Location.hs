module VForth.Location (
   Location(..)
 ) where

data Location = Location {
    title :: String
  , description :: String
  }

instance Show Location where
  show l = title l ++ "\n"
         ++ replicate (length $ title l) '-' ++ "\n"
         ++ description l
