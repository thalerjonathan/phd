module SysDynSIR.Init (
    createSysDynSIR
  ) where

import SysDynSIR.Model
import SysDynSIR.StockFlow

import FRP.Yampa

import FRP.FrABS

import System.Random

createSysDynSIR :: IO ([SysDynSIRDef], SysDynSIREnvironment)
createSysDynSIR =  
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

        return (adefs, ())

createStock :: AgentId
                -> SysDynSIRStockState
                -> SysDynSIRStockBehaviour
                -> IO SysDynSIRDef
createStock stockId stockState stockBeh = 
    do
        -- NOTE: we need a rng to create the definition but we dont create a new one as SystemDynamics does not rely on RNGs
        rng <- getStdGen

        return AgentDef { adId = stockId,
                            adState = stockState,
                            adBeh = ignoreEnv (stockBeh stockState),
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adRng = rng }

createFlow :: AgentId
                -> SysDynSIRFlowBehaviour
                -> IO SysDynSIRDef
createFlow flowId flowBeh = 
    do
        -- NOTE: we need a rng to create the definition but we dont create a new one as SystemDynamics does not rely on RNGs
        rng <- getStdGen

        return AgentDef { adId = flowId,
                            adState = 0.0, -- NOTE: a flow does not has or use its state, set it to dummy value 0
                            adBeh = ignoreEnv flowBeh,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------