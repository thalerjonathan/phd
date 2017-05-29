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
    azAgentMemory :: [Double]
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
destructionRadius = 4.0

sampleRadius :: Double
sampleRadius = 4.0
------------------------------------------------------------------------------------------------------------------------

randomAgent :: (AgentId, EnvCoord)
                -> AgentZeroAgentBehaviour
                -> Rand StdGen AgentZeroAgentDef
randomAgent (agentId, coord) beh = 
    do
        rng <- getSplit

        let s = AgentZeroAgentState {
          azAgentAffect = 0.0,
          azAgentLearningRate = 0.0,
          azAgentLambda = 0.0,
          azAgentDelta = 0.0,
          azAgentThresh = 0.0,
          azAgentEventCount = 0,
          azAgentDispo = 0.0,
          azAgentProb = 0.0,
          azAgentMemory = []
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