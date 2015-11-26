module Player where

data Player =
  Player {
    username    :: String
  , email       :: String
  , displayName :: String
  } deriving Show
