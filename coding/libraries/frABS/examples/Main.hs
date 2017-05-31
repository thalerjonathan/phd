module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun

import SIRS.RunSIRS
import Segregation.SegregationRun

import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import MessageSampling.MessageSamplingRun

import DoubleAuction.DARun

main :: IO ()
main = runDoubleAuctionWithRendering

    -- runAgentZeroWithRendering
    -- runMessageSamplingAndPrint
    -- runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering
    -- runSegStepsAndRender
    -- test
    -- testGloss


{-
    do
        let ret = testMaybeMonad "Default"
        putStrLn $ show ret

testMaybeMonad :: String -> Maybe String
testMaybeMonad d =
    do
        i <- maybeFunc1
        b <- maybeFunc2
        s <- maybeFunc3
        return s

maybeFunc1 :: Maybe Int
maybeFunc1 = Just 42

maybeFunc2 :: Maybe Bool
maybeFunc2 = Nothing

maybeFunc3 :: Maybe String
maybeFunc3 = Just "Jonathan"
-}