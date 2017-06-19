module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun

import SIRS.RunSIRS
import Segregation.SegregationRun

import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import MessageSampling.MessageSamplingRun

import DoubleAuction.DARun

import Wildfire.WildfireRun

main :: IO ()
main = runWildfireWithRendering 

    -- runDoubleAuctionWithRendering
    -- runAgentZeroWithRendering
    -- runMessageSamplingAndPrint
    -- runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering
    -- runSegStepsAndRender