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
	- state-monadic implementation for
		- Agent-Zero
		- SugarScape

	- Yampa arrowized implementation for
		- DoubleAuction: no use of time-semantics but to get used to arrowized programming
		- Agent-Zero : using time-semantics in the environment
		
	- reuse the Agent2D renderer if appropriate
	- use-case for continuous 2d-environment: implement Heroes & Cowards

	- Model-specification using QuickCheck
	- comment haskell-code
	- clean-up imports
	- clean-up structure: lint
-}

main :: IO ()
main = runSIRSWithRendering

	-- runDoubleAuctionSteps
	-- runSIRSWithRendering
	-- runPDWithRendering
	-- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
   	-- runMetaABSStepsAndPrint
   	-- runSegWithRendering