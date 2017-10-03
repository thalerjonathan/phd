module Main where

import           AgentZero.Run
import           Conversation.Run
import           DoubleAuction.Run
import           FrSIRSNetwork.Run
import           FrSIRSSpatial.Run
import           HeroesCowards.Run
import           PolicyEffects.Run
import           PrisonersDilemma.Run
import           RecursiveABS.Run
import           Segregation.Run
import           SIRS.Run
import           SugarScape.Run
import           SysDynSIR.Run
import           Wildfire.Run
import           Zombies.Run
import           NewAgents.Run

{- TODOs
	- implememt parallelstrategy using SF instead my own implementation
	- make current simulation time to rendering and step callbacks
	- better support SD programming: an own module with the typesynonyms and functions defined
		-> define separate module SD with Stock & Flow definitions. Also top-level function which runs SD: e.g. runSD without graphical output (for now)
	- different build targets: with/without time-traveling, gloss rendering
	- add how-to get FrABS examples running on github in a sandbox: need yampa with exposed core, haskell-titan, install FrABS
	
    - explore reasoning testing of ABS using FrSIRSSpatial/Network
        -> reasoning: spatial and network behaviour is EXACTLY the same except selection of neighbours => what can we reason about it regarding the dynamics?
        -> testing replies: an infected agent always replies with contact infected, a recovered never replies, a susceptible never replies
        -> testing contacts: a susceptible contacts occaaionally, an infected contacts occasionally, a recovered never contacts.
        -> testing: an infected agent recovering after (same as falling ball?)
        -> testing: a susceptible agent gets infected after infect contact
        -> testing: can we measure the occasional distribution to verify?
        -> testing: maybe our FrABS solution is even smoother and closer to the analytical solution

	- clean-up, can all be done in one rush through ALL the files:
		-> STYLE, INDENTATIONS & COMMENTS:		https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
		-> WARNINGS:							no warnings with -Wall -Werror
	
-}

main :: IO ()
main = runFrSIRSNetworkReplicationsAndWriteToFile -- runFrSIRSNetworkStepsAndWriteToFile --runFrSIRSNetworkReplicationsAndWriteToFile

    -- runZombiesWithRendering
    -- runPolicyEffectsWithRendering
    -- runFrSIRSNetworkStepsAndWriteToFile -- runFrSIRSNetworkWithRendering -- runFrSIRSNetworkReplicationsAndWriteToFile
    -- runSysDynSIRStepsAndWriteToFile
    -- runFrSIRSSpatialWithRendering -- runFrSIRSSpatialStepsAndPrint -- runFrSIRSSpatialStepsAndWriteToFile
    -- runDoubleAuctionSteps runDoubleAuctionDebug
    -- runSIRSWithRendering
    -- runPDWithRendering
    -- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
    -- runMetaABSStepsAndPrint
    -- runSegWithRendering
