{-# LANGUAGE DeriveGeneric #-}
module Main where

import System.Environment (getArgs, lookupEnv)
import SearchApp
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Text.RegexPR (gsubRegexPR)
import Data.Maybe (fromJust)

newtype GoogleResponse = GoogleResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

getResponse :: String -> String -> IO (Response GoogleResponse)
getResponse initial key = do
    let query = gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb"
    request <- S.parseRequest requestScheme
    S.httpJSON request

main = do 
    key <- lookupEnv "GOOGLE_API_KEY"
    let apiKey = fromJust key
    args <- getArgs
    body <- (getResponse (head args) apiKey) :: IO (Response GoogleResponse)
    let links = getResponseBody body

    let initialState = State {
        _mytems = (items links),
        _cursorPos = 0,
        _cursorScroll = 0,
        _ohNo = ""}

    let app = B.App {
        appStartEvent = return (),
        appChooseCursor = neverShowCursor,
        appDraw = drawUI,
        appHandleEvent = handleEvent,
        appAttrMap = getAttrMap} :: B.App State () Name
    newState <- defaultMain app initialState
    print $ _ohNo newState