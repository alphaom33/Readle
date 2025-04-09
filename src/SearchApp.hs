{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SearchApp where

import Data.Maybe
import System.Environment
import System.IO
import Data.Char
import Text.JSON.Generic
import GHC.Generics
import Control.Monad
import Control.Monad.State.Class
import Text.Wrap
import Debug.Trace

import Brick
import Brick.Main (App)

import Graphics.Vty

import Lens.Micro.Mtl
import Lens.Micro.TH
import Lens.Micro;

data Item = Item {
    title :: String,
    link :: String,
    snippet :: String
} deriving (Show, Generic)

data State = State {
    _mytems :: [Item],
    _cursorPos :: Int,
    _cursorScroll :: Int,
    _ohNo :: String
} deriving Show
$(makeLenses ''State)

wrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst}

boxThing :: Int -> Item -> Widget Name
boxThing index item =
    let 
        el yep = yep <=> strWrapWith wrapSettings ("  " ++ snippet item) <=> str " "
        label = str (title item)
    in
        if index == 0 then el (withAttr (attrName "current") label) else el label

drawUI :: State -> [Widget Name]
drawUI state = [viewport Viewport1 Vertical $ vBox (zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state))]

getAttrMap :: State -> AttrMap
getAttrMap _ = attrMap defAttr [(attrName "current", bg blue)]

wrap :: (Int -> Int) -> State -> State
wrap func state = 
    let
        cur = func $ view cursorPos state
        top = (fromIntegral $ length $ view mytems state) - 1
        wrap = min top $ max 0 cur
    in 
        set cursorPos wrap state

data Name = Viewport1 deriving (Show, Eq, Ord)

checkCur :: Int -> Int -> Int
checkCur scroll cur =
    let
        diff = cur - (scroll `div` 3)
        a = print diff
    in
        if diff > 3 || diff < 1 then signum diff * 3 else 0

handleEvent :: BrickEvent Name () -> EventM Name State ()
handleEvent (VtyEvent event) =
    let
        ack func = do
            modify $ wrap func

            newPos <- use cursorPos
            scroll <- use cursorScroll
            let scrollby = checkCur scroll newPos

            cursorScroll %= (+scrollby)
            vScrollBy (viewportScroll (Viewport1)) $ scrollby
    in
        case event of
            EvKey KEsc [] -> halt
            EvKey (KChar 'j') [] -> ack (+1)
            EvKey (KChar 'k') [] -> ack (subtract 1)
            EvKey kEnter [] -> do
                curPos <- use cursorPos
                mytems <- use mytems
                ohNo .= link (mytems!!curPos)
                halt
            _ -> return ()

handleEvent _ = return ()

initialState = State {
    _mytems = [
        Item {title="asdf", link="a", snippet="snippetyyaya"},
        Item {title="vsdf", link="b", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="c", snippet="snippetyyaya"},
        Item {title="vsdf", link="d", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="e", snippet="snippetyyaya"},
        Item {title="vsdf", link="f", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="g", snippet="snippetyyaya"},
        Item {title="vsdf", link="h", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="i", snippet="snippetyyaya"},
        Item {title="vsdf", link="j", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="k", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="l", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="l", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="l", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="l", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"},
        Item {title="asdf", link="l", snippet="snippetyyaya"},
        Item {title="vsdf", link="l", snippet="znxcvjuoejilageownues"}],
    _cursorPos = 0,
    _cursorScroll = 0,
    _ohNo = ""}