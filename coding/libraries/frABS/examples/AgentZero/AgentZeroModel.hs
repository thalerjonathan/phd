module AgentZero.AgentZeroModel (
    AgentZeroMsg (..),
    AgentZeroAgentState (..),
    AgentZeroCellState (..),
    AgentZeroEnvCell (..),

    AgentZeroLink,
    AgentZeroEnvironment,
    AgentZeroEnvironmentBehaviour,

    AgentZeroAgentDef,
    AgentZeroAgentBehaviour,
    AgentZeroAgentIn,
    AgentZeroAgentOut,

    extinctionRate,
    destructionRadius,
    sampleRadius,
    memorySize,
    
    createAgentZero
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

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
    azAgentMemory :: [Double]
} deriving (Show)

data AgentZeroCellState = Friendly | Attack | Dead deriving (Eq, Show)

data AgentZeroEnvCell = AgentZeroEnvCell {
    azCellState :: AgentZeroCellState,
    azCellShade :: Float
} deriving (Show)

type AgentZeroLink = Double
type AgentZeroEnvironment = Environment AgentZeroEnvCell AgentZeroLink
type AgentZeroEnvironmentBehaviour = EnvironmentBehaviour AgentZeroEnvCell AgentZeroLink

type AgentZeroAgentDef = AgentDef AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell AgentZeroLink
type AgentZeroAgentBehaviour = AgentBehaviour AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell AgentZeroLink
type AgentZeroAgentIn = AgentIn AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell AgentZeroLink
type AgentZeroAgentOut = AgentOut AgentZeroAgentState AgentZeroMsg AgentZeroEnvCell AgentZeroLink
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
                    -> AgentZeroAgentBehaviour 
                    -> Rand StdGen AgentZeroAgentDef
createAgentZero (agentId, coord) beh = 
    do
        rng <- getSplit

        let s = AgentZeroAgentState {
          azAgentAffect = 0.001,
          azAgentLearningRate = 0.1,
          azAgentLambda = 1.0,
          azAgentDelta = 0.0,
          azAgentThresh = 0.5,
          azAgentEventCount = 0,
          azAgentDispo = 0.0,
          azAgentProb = 0.0,
          azAgentMemory = replicate memorySize 0.0
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