{-# LANGUAGE DeriveGeneric #-}
module Network where

import System.Environment (getArgs, lookupEnv)
import SearchApp
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Text.RegexPR (gsubRegexPR)
import Data.Maybe (fromJust)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form(formFocus, formState), handleFormEvent)
import Lens.Micro.Extras (view)
import Message (Message(NewSearch, NextPage, LastPage))
import Control.Monad (join)
import Data.Text (Text, unpack)


newtype GoogleResponse = GoogleResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

getResponse :: String -> String -> Int -> IO (Response GoogleResponse)
getResponse initial key start = do
    let query = gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb" ++ "&start=" ++ show start
    request <- S.parseRequest requestScheme
    S.httpJSON request
