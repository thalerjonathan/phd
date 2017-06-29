module SysDynSIRS.SysDynSIRSModel (
    SysDynSIRSMsg (..),
    SysDynSIRSStockState (..),

    SysDynSIRSEnvLink,
    SysDynSIRSEnvCell,
    SysDynSIRSEnvironment,

    SysDynSIRSDef,
    SysDynSIRSBehaviour,
    SysDynSIRSIn,
    SysDynSIRSOut,

    totalPopulation,
    infectivity,
    contactRate,
    avgIllnessDuration,

    susceptibleStockId,
    infectiousStockId,
    recoveredStockId,
    infectionRateFlowId,
    recoveryRateFlowId
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SysDynSIRSMsg = Flow Double | Stock Double deriving (Eq, Show)

-- NOTE: the flows are stateless, state is only used by the Stocks 
type SysDynSIRSStockState = Double

type SysDynSIRSEnvLink = ()
type SysDynSIRSEnvCell = ()
type SysDynSIRSEnvironment = Environment SysDynSIRSEnvCell SysDynSIRSEnvLink

type SysDynSIRSDef = AgentDef SysDynSIRSStockState SysDynSIRSMsg SysDynSIRSEnvCell SysDynSIRSEnvLink
type SysDynSIRSBehaviour = AgentBehaviour SysDynSIRSStockState SysDynSIRSMsg SysDynSIRSEnvCell SysDynSIRSEnvLink
type SysDynSIRSIn = AgentIn SysDynSIRSStockState SysDynSIRSMsg SysDynSIRSEnvCell SysDynSIRSEnvLink
type SysDynSIRSOut = AgentOut SysDynSIRSStockState SysDynSIRSMsg SysDynSIRSEnvCell SysDynSIRSEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS 
-- NOTE: in SystemDynamics all parameters are in floating-point even population-sizes
totalPopulation :: Double
totalPopulation = 1000

-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 5

avgIllnessDuration :: Double
avgIllnessDuration = 15
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Hard-coded ids for flows (=sending messages)
susceptibleStockId :: AgentId
susceptibleStockId = 0

infectiousStockId :: AgentId
infectiousStockId = 1

recoveredStockId :: AgentId
recoveredStockId = 2

infectionRateFlowId :: AgentId
infectionRateFlowId = 3

recoveryRateFlowId :: AgentId
recoveryRateFlowId = 4
------------------------------------------------------------------------------------------------------------------------