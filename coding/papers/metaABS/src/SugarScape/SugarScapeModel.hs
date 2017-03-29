{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random


-- TODO Implement SugarScape Chapters
    -- TODO environment also needs a signalfunction which runs once per simulation-step: environmentIn/out
    -- TODO test creation / killing of agents
    -- TODO random iteration in sequential

-- TODO implement rules as SF which can be turned on or off
-- TODO formalize rules in my EDSL
-- TODO problem of sugarscape trading in our functional approach: cannot reply immediately thus potentially violating budget constraints. need to solve this e.g. by having a temporary reserved amount "open for transaction"

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type SugarScapeMsg = ()

data SugarScapeAgentState = SugarScapeAgentState {
    sugTribe :: Int,
    sugRng :: StdGen
} deriving (Show)

data SugarScapeEnvCell = SugarScapeEnvCell {
    sugEnvSugarCapacity :: Double,
    sugEnvSugarLevel :: Double
}

type SugarScapeEnvironment = Environment SugarScapeEnvCell
type SugarScapeEnvironmentBehaviour = EnvironmentBehaviour SugarScapeEnvCell

type SugarScapeAgentDef = AgentDef SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentBehaviour = AgentBehaviour SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentIn = AgentIn SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentOut = AgentOut SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
sugarGrowbackRate :: Double
sugarGrowbackRate = 2.0

sugarMinCapacity :: Double
sugarMinCapacity = 1.0

sugarMaxCapacity :: Double
sugarMaxCapacity = 10.0
------------------------------------------------------------------------------------------------------------------------


sugarScapeEnvironmentBehaviour :: SugarScapeEnvironmentBehaviour
sugarScapeEnvironmentBehaviour env = env

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< aout