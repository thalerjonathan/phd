module SysDynSIR.Model (
    SysDynSIRMsg (..),
    SysDynSIRStockState (..),

    SysDynSIREnvLink,
    SysDynSIREnvCell,
    SysDynSIREnvironment,

    SysDynSIRDef,
    SysDynSIRBehaviour,
    SysDynSIRIn,
    SysDynSIROut,

    SysDynSIRFlowBehaviour,
    SysDynSIRStockBehaviour,

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
data SysDynSIRMsg = Value Double deriving (Eq, Show)

-- NOTE: the flows are stateless, state is only used by the Stocks 
type SysDynSIRStockState = Double

type SysDynSIREnvLink = ()
type SysDynSIREnvCell = ()
type SysDynSIREnvironment = Environment SysDynSIREnvCell SysDynSIREnvLink

type SysDynSIRDef = AgentDef SysDynSIRStockState SysDynSIRMsg SysDynSIREnvCell SysDynSIREnvLink
type SysDynSIRBehaviour = AgentBehaviour SysDynSIRStockState SysDynSIRMsg SysDynSIREnvCell SysDynSIREnvLink
type SysDynSIRIn = AgentIn SysDynSIRStockState SysDynSIRMsg SysDynSIREnvCell SysDynSIREnvLink
type SysDynSIROut = AgentOut SysDynSIRStockState SysDynSIRMsg SysDynSIREnvCell SysDynSIREnvLink

type SysDynSIRFlowBehaviour = SysDynSIRBehaviour
type SysDynSIRStockBehaviour = Double -> SysDynSIRBehaviour
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS 
-- NOTE: in SystemDynamics all parameters are in floating-point even population-sizes
totalPopulation :: Double
totalPopulation = 1024

-- average probability of getting infected 
infectivity :: Double
infectivity = 0.05

-- average number of contacts per time-unit
contactRate :: Double
contactRate = 7

avgIllnessDuration :: Double
avgIllnessDuration = 15
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Hard-coded ids for stocks & flows interaction (sending messages)
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