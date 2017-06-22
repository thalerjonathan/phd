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

{-
	TODOs
	- export only public functions in FrABS
	- Monadic implementations using State
	- Time-semantics of Yampa using Arrowized implementations 
	- use-case for continuous 2d-environment: implement Heroes & Cowards
	- Model-specification using QuickCheck
	- comment haskell-code
	- clean-up imports
	- clean-up structure: lint
-}

main :: IO ()
main = runSIRSWithRendering

	-- TODO: apply gloss-rendering to the next ones
	-- runSIRSWithRendering
	-- runConversationStepsAndPrint
	-- runDoubleAuctionWithRendering
	-- runMetaABSStepsAndPrint

	-- runPDWithRendering
	-- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runSegWithRendering