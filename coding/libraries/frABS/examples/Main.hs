module Main where

import           AgentZero.Run
import           Conversation.Run
import           DoubleAuction.Run
import           FrSIR.Run
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
	- different build targets: with/without time-traveling, gloss rendering
	- add HOW-TO of FrABS examples running on github in a sandbox: need yampa with exposed core, haskell-titan, install FrABS
	
	- clean-up, can all be done in one rush through ALL the files:
		-> STYLE, INDENTATIONS & COMMENTS:		https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
		-> WARNINGS:							no warnings with -Wall -Werror
	
-}

main :: IO ()
main = runHeroesCowardsWithRendering -- runFrSIRReplicationsAndWriteToFile -- runFrSIRStepsAndWriteToFile

    -- runFrSIRReplicationsAndWriteToFile -- runFrSIRStepsAndWriteToFile
    -- runZombiesWithRendering
    -- runPolicyEffectsWithRendering
    -- runFrSIRSNetworkStepsAndWriteToFile -- runFrSIRSNetworkWithRendering -- runFrSIRSNetworkReplicationsAndWriteToFile
    -- runSysDynSIRStepsAndWriteToFile
    -- runFrSIRSSpatialWithRendering -- runFrSIRSSpatialStepsAndPrint -- runFrSIRSSpatialStepsAndWriteToFile -- runFrSIRSSpatialStepsAndRender
    -- runDoubleAuctionSteps runDoubleAuctionDebug
    -- runSIRSWithRendering
    -- runPDWithRendering
    -- runWildfireWithRendering
    -- runAgentZeroWithRendering
    -- runSugarScapeWithRendering
    -- runConversationSteps
    -- runMetaABSStepsAndPrint
    -- runSegWithRendering