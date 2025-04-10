{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Webpage
import qualified SearchApp
import qualified HTMLParser

import System.Environment (getArgs, lookupEnv)
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Text.RegexPR as R
import Data.Maybe (fromJust)
import Control.Monad (join)
import qualified Data.ByteString.Lazy.Char8 as L8

newtype GoogleResponse = GoogleResponse {
    items :: [SearchApp.Item]
} deriving (Show, Generic)

instance FromJSON SearchApp.Item
instance ToJSON SearchApp.Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

getResponse :: String -> String -> IO (Response GoogleResponse)
getResponse initial key = do
    let query = R.gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb"
    request <- S.parseRequest requestScheme
    S.httpJSON request

main = do 
    print $ HTMLParser.parseString "<aasdf8></a>"
    -- let codes = R.multiMatchRegexPR "(?<=<code>).*?(?=</code>)" $ L8.unpack html
    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key
    -- args <- getArgs
    -- body <- (getResponse (head args) apiKey) :: IO (Response GoogleResponse)
    -- let links = getResponseBody body

    -- let initialState = Search.State {
    --     _mytems = (items links),
    --     _cursorPos = 0,
    --     _cursorScroll = 0,
    --     _ohNo = ""}

    -- let app = B.App {
    --     appStartEvent = return (),
    --     appChooseCursor = neverShowCursor,
    --     appDraw = drawUI,
    --     appHandleEvent = handleEvent,
    --     appAttrMap = getAttrMap} :: B.App Search.State () Search.Name
    -- newState <- defaultMain app initialState
    -- newState <- defaultMain Webpage.app Webpage.initialState
