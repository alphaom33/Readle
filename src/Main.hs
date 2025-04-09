{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Network.HTTP.Simple
import Text.RegexPR
import Text.JSON.Generic
import Data.Aeson
import GHC.Generics
import Control.Monad
import Text.Wrap
import Debug.Trace

import Brick
import Brick.Main (App)

import Graphics.Vty

import Lens.Micro.Mtl
import Lens.Micro.TH

data Item = Item {
    title :: String,
    link :: String,
    snippet :: String
} deriving (Show, Generic)

newtype GoogleResponse = GoogleResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

wrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst}

boxThing :: Item -> Widget ()
boxThing item = str (title item) <=> strWrapWith wrapSettings ("  " ++ snippet item) <=> str " "

drawUI :: MyAppState -> [Widget ()]
drawUI state = [str (_ohno state)]

ui :: [Item] -> Widget ()
ui items = vBox (map boxThing items)

    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key

    -- args <- getArgs

    -- body <- (getResponse (head args) apiKey) :: IO (Response GoogleResponse)
    -- let links = getResponseBody body

getAttrMap :: MyAppState -> AttrMap
getAttrMap _ = attrMap defAttr [(attrName "asdf", bg blue)]

data State = State {
    mytems :: [Item],
    cursorPos :: Integer,
    ohNo :: String
} deriving Show

data MyAppState = MyAppState {_ohno :: String}

handleEvent :: BrickEvent () () -> EventM () MyAppState ()
handleEvent (VtyEvent event) = do
    case event of
        EvKey KEsc [] -> halt
        EvKey KEnter [] -> put MyAppState {_ohno = "asdf"}
        _ -> return ()

handleEvent _ = return ()

main = let app = Brick.App {
                appStartEvent = return (),
                appChooseCursor = neverShowCursor,
                appDraw = drawUI,
                appHandleEvent = handleEvent,
                appAttrMap = getAttrMap} :: Brick.App MyAppState () ()
           initialState = MyAppState {_ohno = ""}
           in 
           defaultMain app initialState

getResponse :: String -> String -> IO (Response GoogleResponse)
getResponse initial key = do
    let query = gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb"
    request <- parseRequest requestScheme
    httpJSON request
