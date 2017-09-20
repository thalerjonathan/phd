module SysDynSIR.Model (
    SysDynSIRMsg (..),
    SysDynSIRStockState (..),

    SysDynSIREnvironment,

    SysDynSIRDef,
    SysDynSIRBehaviour,
    SysDynSIRIn,
    SysDynSIROut,
    SysDynSIRObservable,
    
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

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SysDynSIRMsg = Value Double deriving (Eq, Show)
type SysDynSIRStockState = Double

type SysDynSIREnvironment = ()

type SysDynSIRDef = AgentDef SysDynSIRStockState SysDynSIRMsg SysDynSIREnvironment 
type SysDynSIRBehaviour = ReactiveBehaviourIgnoreEnv SysDynSIRStockState SysDynSIRMsg SysDynSIREnvironment 
type SysDynSIRIn = AgentIn SysDynSIRStockState SysDynSIRMsg SysDynSIREnvironment 
type SysDynSIROut = AgentOut SysDynSIRStockState SysDynSIRMsg SysDynSIREnvironment 
type SysDynSIRObservable = AgentObservable SysDynSIRStockState

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