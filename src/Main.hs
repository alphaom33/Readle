{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Network.HTTP.Simple
import Text.RegexPR
import Text.JSON.Generic
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Control.Monad
import Text.Wrap
import Debug.Trace

import Brick
import Brick.Main (App)

import Graphics.Vty

import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro;

data Item = Item {
    _title :: String,
    _link :: String,
    _snippet :: String
} deriving (Show, Generic)

newtype GoogleResponse = GoogleResponse {
    _items :: [Item]
} deriving (Show, Generic)

instance FromJSON Item
instance ToJSON Item

instance FromJSON GoogleResponse
instance ToJSON GoogleResponse

$(makeLenses ''GoogleResponse)
$(makeLenses ''Item)


data State = State {
    _mytems :: [Item],
    _cursorPos :: Integer,
    _ohNo :: String
} deriving Show
$(makeLenses ''State)

wrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst}

boxThing :: Integer -> Item -> Widget ()
boxThing index item =
    let 
        el yep = yep <=> strWrapWith wrapSettings ("  " ++ view snippet item) <=> str " "
        label = str (view title item)
    in
        if index == 0 then el (withAttr (attrName "current") label) else el label

drawUI :: State -> [Widget ()]
drawUI state = [vBox (zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state))]

    -- key <- lookupEnv "GOOGLE_API_KEY"
    -- let apiKey = fromJust key

    -- args <- getArgs

    -- body <- (getResponse (head args) apiKey) :: IO (Response GoogleResponse)
    -- let links = getResponseBody body

getAttrMap :: State -> AttrMap
getAttrMap _ = attrMap defAttr [(attrName "current", bg blue)]

handleEvent :: BrickEvent () e -> EventM () State ()
handleEvent (VtyEvent event) = do
    case event of
        EvKey KEsc [] -> halt
        EvKey (KChar 'j') [] -> cursorPos %= (+1)
        EvKey (KChar 'k') [] -> cursorPos %= subtract 1
        _ -> return ()

handleEvent _ = return ()

main = let app = Brick.App {
                appStartEvent = return (),
                appChooseCursor = neverShowCursor,
                appDraw = drawUI,
                appHandleEvent = handleEvent,
                appAttrMap = getAttrMap} :: Brick.App State () ()
           initialState = State {_mytems = [
            Item {_title="asdf", _link="l", _snippet="snippetyyaya"},
            Item {_title="asdf", _link="l", _snippet="snippetyyaya"},
            Item {_title="asdf", _link="l", _snippet="snippetyyaya"}
           ], _cursorPos = 0, _ohNo = ""}
           in 
           defaultMain app initialState

getResponse :: String -> String -> IO (Response GoogleResponse)
getResponse initial key = do
    let query = gsubRegexPR " " "+" initial
    let requestScheme = "https://www.googleapis.com/customsearch/v1?q=" ++ query ++ "&key=" ++ key ++ "&cx=1433b113b742d4cdb"
    request <- parseRequest requestScheme
    httpJSON request
