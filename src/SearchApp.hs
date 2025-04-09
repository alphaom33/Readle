{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SearchApp where

import Text.Wrap (WrapSettings(..), FillScope(..), FillStrategy(..))
import GHC.Generics (Generic)

import Brick as B
import Brick.Main (App)

import Graphics.Vty as V

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set)

import Control.Monad (join)
import Data.Maybe (fromJust)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style

data Item = Item {
    title :: String,
    link :: String,
    snippet :: String
} deriving (Show, Generic)

data State = State {
    _mytems :: [Item],
    _cursorPos :: Int,
    _cursorScroll :: Int,
    _ohNo :: String,
    _canMove :: Bool
} deriving Show
$(makeLenses ''State)

wrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst}

makeSearch :: B.Widget Name
makeSearch = withAttr (attrName "searchBox") $ hCenter $ border $ str "dsaf"

--TODO variable dst
boxThing :: Int -> Item -> B.Widget Name
boxThing index item =
    let 
        el yep = yep <=> str ("  " ++ snippet item) <=> str " "
        label = str (title item)
    in
        if index == 0 then el (withAttr (attrName "current") label) else el label

makeBoxes :: State -> [B.Widget Name]
makeBoxes state = zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state)

drawUI :: State -> [B.Widget Name]
drawUI state = (if not $ view canMove state then (makeSearch:) else id) [viewport Viewport1 Vertical $ 
    vBox $ makeBoxes state]

getAttrMap :: State -> B.AttrMap
getAttrMap state = attrMap defAttr [
    (attrName "current", if view canMove state then bg blue else defAttr),
    (attrName "searchBox", if not $ view canMove state then fg white else fg black)]

wrap :: (Int -> Int) -> State -> State
wrap func state = 
    let
        cur = func $ view cursorPos state
        top = fromIntegral  (length $ view mytems state) - 1
        wrap = min top $ max 0 cur
    in 
        set cursorPos wrap state

data Name = Viewport1 deriving (Show, Eq, Ord)

checkCur :: Int -> Int -> Int -> Int
checkCur scroll cur height
    | diff > (adjustedHeight - scrollBarrier) = 3
    | diff < scrollBarrier = -3
    | otherwise = 0
    where 
        scrollBarrier = 1
        diff = cur - (scroll `div` 3)
        adjustedHeight = height `div` 3

moveCursor :: (Int -> Int) -> EventM Name State ()
moveCursor func = do
    modify $ wrap func

    view <- lookupViewport Viewport1
    let size = fromJust view
    let height = regionHeight $ _vpSize size

    newPos <- use cursorPos
    scroll <- use cursorScroll
    let scrollby = checkCur scroll newPos height

    cursorScroll %= (+scrollby)
    vScrollBy (viewportScroll Viewport1) scrollby

handleEvent :: B.BrickEvent Name () -> B.EventM Name State ()
handleEvent (B.VtyEvent event) =
    case event of
        V.EvKey V.KEsc [] -> halt
        V.EvKey (V.KChar 'j') [] -> moveCursor (+1)
        V.EvKey (V.KChar 'k') [] -> moveCursor (subtract 1)
        V.EvKey V.KEnter [] -> do
            curPos <- use cursorPos
            mytems <- use mytems
            ohNo .= link (mytems!!curPos)
            halt
        V.EvKey (V.KChar '=') [] -> canMove %= not
        _ -> return ()

handleEvent _ = return ()
