{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Webpage where

import Text.Wrap (WrapSettings(..), FillScope(..), FillStrategy(..))
import GHC.Generics (Generic)

import Brick as B
import Brick.Main (App)

import Graphics.Vty as V

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set)

data State = State {
    _html :: String
} deriving (Generic, Show)
$(makeLenses ''State)

drawUI :: State -> [Widget ()]
drawUI e = [str $ view html e]

handleEvent :: B.BrickEvent () () -> B.EventM () State ()
handleEvent e = return ()

getAttrMap :: State -> B.AttrMap
getAttrMap _ = attrMap defAttr []

initialState page = State {
    _html = page
}

app = B.App {
    appStartEvent = return (),
    appChooseCursor = neverShowCursor,
    appDraw = drawUI,
    appHandleEvent = handleEvent,
    appAttrMap = getAttrMap} :: B.App State () ()
