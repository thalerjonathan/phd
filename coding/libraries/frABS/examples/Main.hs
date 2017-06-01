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

    -- runDoubleAuctionWithRendering
    -- runAgentZeroWithRendering
    -- runMessageSamplingAndPrint
    -- runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering
    -- runSegStepsAndRender