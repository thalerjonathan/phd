module Main where

import AgentZero.AgentZeroRun
import SugarScape.SugarScapeRun
import SIRS.SIRSRun
import Segregation.SegregationRun
import RecursiveABS.RecursiveABSRun
import Conversation.ConversationRun
import DoubleAuction.DARun
import Wildfire.WildfireRun
import PrisonersDilemma.PDRun

{-
	TODOs
	- export only public functions in FrABS
	- reuse the Agent2D renderer if appropriate
	- Monadic implementations using State
	- Time-semantics of Yampa using Arrowized implementations 
	- use-case for continuous 2d-environment: implement Heroes & Cowards
	- Model-specification using QuickCheck
	- comment haskell-code
	- clean-up imports
	- clean-up structure: lint
-}

main :: IO ()
main = runSegWithRendering

	-- runDoubleAuctionSteps
	-- runSIRSWithRendering
	-- runPDWithRendering
	-- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
   	-- runMetaABSStepsAndPrint
   	-- runSegWithRendering