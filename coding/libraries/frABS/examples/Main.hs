module Main where

import           Conversation.Run
import           DoubleAuction.Run
import           FrSIRSSpatial.Run
import           HeroesCowards.Run
import           PrisonersDilemma.Run
import           RecursiveABS.Run
import           Segregation.Run
import           SIRS.Run
-- import           SugarScape.Run
import           SysDynSIR.Run
import           Wildfire.Run

{-
import           AgentZero.Run
import           FrSIRSNetwork.Run
import           PolicyEffects.Run
-}

{-
    TODOs
    - GlossSimulator: pass Background-color and Circle/Rectangle as additional parameters (use currying)
    - clean-up
        - imports: no unused imports
        - lint: must be clear of warnings
        - warnings: compilation with -w must show no warnings at all
        
    - comment haskell code
-}

main :: IO ()
main = runHeroesCowardsWithRendering

    -- runPolicyEffectsWithRendering
    -- runFrSIRSNetworkStepsAndWriteToFile -- runFrSIRSNetworkWithRendering -- runFrSIRSNetworkReplicationsAndWriteToFile
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