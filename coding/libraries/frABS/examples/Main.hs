module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun
import SIRS.RunSIRS
import Segregation.SegregationRun
import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import DoubleAuction.DARun
import Wildfire.WildfireRun
import PrisonersDilemma.PDRun

main :: IO ()
main = runPDWithRendering

	-- runWildfireWithRendering
    -- runDoubleAuctionWithRendering
    -- runAgentZeroWithRendering
    -- runConversationStepsAndPrint
    -- runSugarScapeWithRendering
    -- runMetaABSStepsAndPrint
    -- runSIRSWithRendering
    -- runSegWithRendering