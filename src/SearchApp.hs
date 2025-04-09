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

boxThing :: Int -> Item -> B.Widget Name
--TODO variable dst
boxThing index item =
    let 
        el yep = yep <=> str ("  " ++ snippet item) <=> str " "
        label = str (title item)
    in
        if index == 0 then el (withAttr (attrName "current") label) else el label

makeBoxes :: State -> [B.Widget Name]
makeBoxes state = (zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state))

drawUI :: State -> [B.Widget Name]
drawUI state = [ viewport Viewport1 Vertical $ 
    vBox $ makeBoxes state]

getAttrMap :: State -> B.AttrMap
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

handleEvent :: B.BrickEvent Name () -> B.EventM Name State ()
handleEvent (B.VtyEvent event) =
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
            V.EvKey V.KEsc [] -> halt
            V.EvKey (V.KChar 'j') [] -> ack (+1)
            V.EvKey (V.KChar 'k') [] -> ack (subtract 1)
            V.EvKey V.KEnter [] -> do
                curPos <- use cursorPos
                mytems <- use mytems
                ohNo .= link (mytems!!curPos)
                halt
            _ -> return ()

handleEvent _ = return ()