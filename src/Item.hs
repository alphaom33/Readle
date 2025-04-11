{-# LANGUAGE DeriveGeneric #-}

module Item where

import GHC.Generics

data Item = Item {
    title :: String,
    link :: String,
    snippet :: String
} deriving (Show, Generic)
