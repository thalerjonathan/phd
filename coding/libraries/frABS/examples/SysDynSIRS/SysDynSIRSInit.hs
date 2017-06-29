module SysDynSIRS.SysDynSIRSInit (
    createSysDynSIRS
  ) where

import SysDynSIRS.SysDynSIRSModel
import SysDynSIRS.SysDynSIRSStock

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createSysDynSIRS :: IO ([SysDynSIRSDef], SysDynSIRSEnvironment)
createSysDynSIRS =  
    do
        -- NOTE: we need a rng to create the environment but we dont create a new one as SystemDynamics does not rely on RNGs
        rng <- getStdGen

        let initialSusceptibleStockValue = totalPopulation - 1
        let initialRnfectiousStockValue = 1
        let initialRecoveredStockValue = 0

        susStock <- createStock susceptibleStockId initialSusceptibleStockValue susceptibleStock
        infStock <- createStock infectiousStockId initialRnfectiousStockValue infectiousStock
        recStock <- createStock recoveredStockId initialRecoveredStockValue recoveredStock
        
        infRateFlow <- createFlow infectionRateFlowId infectionRateFlow
        recRateFlow <- createFlow recoveryRateFlowId recoveryRateFlow
        
        let adefs = [susStock, infStock, recStock, infRateFlow, recRateFlow]

        -- NOTE: we need to create SOME environment but we never access it, its just a dummy one
        let env = (createEnvironment
                        Nothing
                        (0,0)
                        moore
                        ClipToMax
                        []
                        rng
                        Nothing)

        return (adefs, env)

createStock :: AgentId
                -> SysDynSIRSStockState
                -> SysDynSIRSStockBehaviour
                -> IO SysDynSIRSDef
createStock stockId stockState stockBeh = 
    do
        -- NOTE: we need a rng to create the definition but we dont create a new one as SystemDynamics does not rely on RNGs
        rng <- getStdGen

        return AgentDef { adId = stockId,
                            adState = stockState,
                            adBeh = (stockBeh stockState),
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = (0,0),
                            adRng = rng }

createFlow :: AgentId
                -> SysDynSIRSFlowBehaviour
                -> IO SysDynSIRSDef
createFlow flowId flowBeh = 
    do
        -- NOTE: we need a rng to create the definition but we dont create a new one as SystemDynamics does not rely on RNGs
        rng <- getStdGen

        return AgentDef { adId = flowId,
                            adState = 0.0, -- NOTE: a flow does not has or use its state, set it to dummy value 0
                            adBeh = flowBeh,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = (0,0),
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------