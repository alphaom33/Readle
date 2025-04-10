{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module SearchApp where

import Text.Wrap (WrapSettings(..), FillScope(..), FillStrategy(..))
import GHC.Generics (Generic)

import Brick
import Brick.Main (App)

import Graphics.Vty as V

import Lens.Micro.Mtl ((.=), (%=), use, view)
import Lens.Micro.TH (makeLenses)
import Lens.Micro (set, (^.))

import Control.Monad (join, when, unless)
import Data.Maybe (fromJust)
import Brick.Widgets.Center (center, hCenter)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style
import Brick.Forms (editTextField, (@@=), renderForm, newForm, Form (formState, formFocus), handleFormEvent, setFieldValid, FormFieldState (formFieldUpdate))
import Data.Text (Text)
import Brick.Focus (focusRingCursor)
import Message (Message(NewSearch, Error, EnterSite, NextPage, LastPage))

data Item = Item {
    title :: String,
    link :: String,
    snippet :: String
} deriving (Show, Generic)

newtype Search = Search {
    _query :: Text
} deriving (Eq, Ord, Show)
$(makeLenses ''Search)

data Name = Viewport1 | QueryField deriving (Show, Eq, Ord)

data State = State {
    _mytems :: [Item],
    _cursorPos :: Int,
    _cursorScroll :: Int,
    _message :: Message,
    _canMove :: Bool,
    _form :: Form Search () Name
}
instance Show State where
    show a = "_mytems: " ++ show (_mytems a) ++ " _cursorPos: " ++ show (_cursorPos a) ++ " _canMove: " ++ show (_canMove a) ++ "\n"

$(makeLenses ''State)

wrapSettings = WrapSettings {
    preserveIndentation = True,
    breakLongWords = True,
    fillStrategy = NoFill,
    fillScope = FillAfterFirst}

mkForm :: Search -> Form Search e Name
mkForm = newForm [ editTextField query QueryField (Just 1) ]

drawForm :: State -> Widget Name
drawForm state =
    let
        attr = withAttr $ attrName "searchBox"
    in
        attr $ border $ hCenter $ renderForm $ view form state

--TODO variable dst
boxThing :: Int -> Item -> Widget Name
boxThing index item =
    let
        el yep = yep <=> str ("  " ++ snippet item) <=> str " "
        label = str (title item)
    in
        if index == 0 then el (withAttr (attrName "current") label) else el label

makeBoxes :: State -> [Widget Name]
makeBoxes state = zipWith boxThing (map (view cursorPos state-) [0..]) (view mytems state)

drawUI :: State -> [Widget Name]
drawUI state = (if not $ view canMove state then (drawForm state:) else id)
    [viewport Viewport1 Vertical $ vBox $ makeBoxes state]

getAttrMap :: State -> AttrMap
getAttrMap state = attrMap defAttr [
    (attrName "current", if view canMove state then bg blue else defAttr),
    (attrName "searchBox", if not $ view canMove state then fg white else fg black),
    (attrName "focusedFormInputAttr", white `on` blue),
    (attrName "invalidFormInputAttr", red `on` yellow)]

wrap :: (Int -> Int) -> State -> State
wrap func state =
    let
        cur = func $ view cursorPos state
        top = fromIntegral  (length $ view mytems state) - 1
        wrap = min top $ max 0 cur
    in
        set cursorPos wrap state

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

handleEvent :: BrickEvent Name () -> EventM Name State ()
handleEvent event = do
    curCanMove <- use canMove
    let (VtyEvent ev) = event
    case (curCanMove, ev) of
        (_, V.EvKey V.KEsc []) -> halt
        (_, V.EvKey (V.KChar '=') []) -> do
            canMove %= not
            form .= emptyForm

        (True, V.EvKey (V.KChar 'j') []) -> moveCursor (+1)
        (True, V.EvKey (V.KChar 'k') []) -> moveCursor (subtract 1)
        (True, V.EvKey (V.KChar 'h') []) -> do
            message .= LastPage
            halt
        (True, V.EvKey (V.KChar 'l') []) -> do
            message .= NextPage
            halt
        (True, V.EvKey V.KEnter []) -> do
            curPos <- use cursorPos
            mytems <- use mytems
            message .= EnterSite (link (mytems!!curPos))
            halt

        (False, V.EvKey V.KEnter []) -> do
            canMove .= True
            message .= NewSearch
            halt
        (False, _) -> zoom form $ handleFormEvent event


emptyForm = mkForm Search {_query=""}

initialState links = State {
    _mytems = links,
    _cursorPos = 0,
    _cursorScroll = 0,
    _message = Error "",
    _canMove = True,
    _form = emptyForm}

app = App {
    appStartEvent = return (),
    appChooseCursor = focusRingCursor formFocus . view form,
    appDraw = drawUI,
    appHandleEvent = handleEvent,
    appAttrMap = getAttrMap} :: App State () Name
