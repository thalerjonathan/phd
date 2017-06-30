module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun
import SIRS.SIRSRun
import FrSIRSSpatial.FrSIRSSpatialRun
import FrSIRSNetwork.FrSIRSNetworkRun
import SysDynSIR.SysDynSIRRun
import Segregation.SegregationRun
import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import DoubleAuction.DARun
import Wildfire.WildfireRun
import PrisonersDilemma.PDRun

{-
	TODOs
    - performance?
    
	- reuse the Agent2D renderer if appropriate
	- use-case for continuous 2d-environment: implement Heroes & Cowards

    - compilation with -w must show no warnings at all
    - clean-up imports
    - clean-up structure: lint
    - comment haskell-code

	- Model-specification using QuickCheck
-}

main :: IO ()
main = runFrSIRSSpatialWithRendering

    -- runFrSIRSNetworkStepsAndWriteToFile
    -- runSysDynSIRStepsAndWriteToFile
    -- runFrSIRSSpatialWithRendering -- runFrSIRSSpatialStepsAndPrint -- runFrSIRSSpatialStepsAndWriteToFile
    -- runDoubleAuctionSteps
    -- runSIRSWithRendering
    -- runPDWithRendering
    -- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
    -- runMetaABSStepsAndPrint
    -- runSegWithRendering