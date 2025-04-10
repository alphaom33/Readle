{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs, lookupEnv)
import SearchApp
import Brick as B
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple as S
import Text.RegexPR (gsubRegexPR)
import Data.Maybe (fromJust)
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form(formFocus, formState), handleFormEvent)
import Lens.Micro.Extras (view)
import Message (Message(NewSearch, NextPage, LastPage))
import Control.Monad (join)
import Data.Text (Text, unpack)
import Network

runSearch :: String -> Int -> String -> IO ()
runSearch key start query = do
    body <- getResponse query key start :: IO (Response GoogleResponse)
    let links = getResponseBody body

    finalState <- defaultMain app $ initialState $ items links
    case _message finalState of
        NewSearch -> runSearch key 0 $ unpack $ _query $ formState $ _form finalState
        NextPage -> runSearch key (start + 10) query
        LastPage -> runSearch key (max 0 (start - 10)) query

main = do 
    key <- lookupEnv "GOOGLE_API_KEY"
    let apiKey = fromJust key
    args <- getArgs
    runSearch apiKey 0 (head args)
