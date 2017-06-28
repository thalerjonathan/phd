module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun
import SIRS.SIRSRun
import FrSIRS.FrSIRSRun
import Segregation.SegregationRun
import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import DoubleAuction.DARun
import Wildfire.WildfireRun
import PrisonersDilemma.PDRun

{-
	TODOs
	- reuse the Agent2D renderer if appropriate
	- use-case for continuous 2d-environment: implement Heroes & Cowards

	- Model-specification using QuickCheck
	- comment haskell-code
	- clean-up imports
	- clean-up structure: lint
-}

main :: IO ()
main = runFrSIRSWithRendering

    -- runDoubleAuctionSteps
    -- runFrSIRSWithRendering
    -- runSIRSWithRendering
    -- runPDWithRendering
    -- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
    -- runMetaABSStepsAndPrint
    -- runSegWithRendering