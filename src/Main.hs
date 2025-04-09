{-# LANGUAGE DeriveGeneric #-}
module Main where

import SearchApp
import Brick
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Network.HTTP.Simple
import Text.RegexPR

-- key <- lookupEnv "GOOGLE_API_KEY"
-- let apiKey = fromJust key

-- args <- getArgs

-- body <- (getResponse (head args) apiKey) :: IO (Response GoogleResponse)
-- let links = getResponseBody body
newtype GoogleResponse = GoogleResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

main = do 
    let app = Brick.App {
        appStartEvent = return (),
        appChooseCursor = neverShowCursor,
        appDraw = drawUI,
        appHandleEvent = handleEvent,
        appAttrMap = getAttrMap} :: Brick.App State () Name
    newState <- defaultMain app initialState
    print $ _ohNo newState


getResponse :: String -> String -> IO (Response GoogleResponse)
getResponse initial key = do
    let query = gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb"
    request <- parseRequest requestScheme
    httpJSON request