module AgentZero.AgentZeroModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import System.Random
import Control.Monad.Random
import Control.Monad
import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as Map

import Debug.Trace
------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------

data AgentZeroMsg =
    Disposition Double 
    deriving (Eq, Show)

data AgentZeroAgentState = AgentZeroAgentState {
    azAgentAffect :: Double,
    azAgentLearningRate :: Double,
    azAgentLambda :: Double,
    azAgentDelta :: Double,
    azAgentThresh :: Double,
    azAgentEventCount :: Int,
    azAgentDispo :: Double,
    azAgentProb :: Double,
    azAgentMemory :: [Double],

    azAgentConnections :: Map.Map AgentId Double
} deriving (Show)

data AgentZeroCellState = Friendly | Attack | Dead deriving (Eq, Show)

data AgentZeroEnvCell = AgentZeroEnvCell {
    azCellState :: AgentZeroCellState,
    azCellShade :: Float
} deriving (Show)

type AgentZeroEnvironment = Environment AgentZeroEnvCell
type AgentZeroEnvironmentBehaviour = EnvironmentBehaviour AgentZeroEnvCell

type AgentZeroAgentDef = AgentDef AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell
type AgentZeroAgentBehaviour = AgentBehaviour AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell
type AgentZeroAgentIn = AgentIn AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell
type AgentZeroAgentOut = AgentOut AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
extinctionRate :: Double
extinctionRate = 1.0

destructionRadius :: Double
destructionRadius = 1.0

sampleRadius :: Double
sampleRadius = 1.0

memorySize :: Int
memorySize = 10
------------------------------------------------------------------------------------------------------------------------

createAgentZero :: (AgentId, EnvCoord) 
                    -> [(AgentId, Double)]
                    -> AgentZeroAgentBehaviour 
                    -> Rand StdGen AgentZeroAgentDef
createAgentZero (agentId, coord) cs beh = 
    do
        rng <- getSplit

        let conn = foldr (\(k, v) mapAcc -> Map.insert k v mapAcc) Map.empty cs

        let s = AgentZeroAgentState {
          azAgentAffect = 0.001,
          azAgentLearningRate = 0.1,
          azAgentLambda = 1.0,
          azAgentDelta = 0.0,
          azAgentThresh = 0.5,
          azAgentEventCount = 0,
          azAgentDispo = 0.0,
          azAgentProb = 0.0,
          azAgentMemory = replicate memorySize 0.0,
          azAgentConnections = conn
        }

        let adef = AgentDef {
           adId = agentId,
           adState = s,
           adEnvPos = coord,
           adConversation = Nothing,
           adInitMessages = NoEvent,
           adBeh = beh,
           adRng = rng }

        return adef