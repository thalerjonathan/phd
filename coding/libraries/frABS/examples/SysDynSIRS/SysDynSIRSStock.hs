{-# LANGUAGE Arrows #-}
module SysDynSIRS.SysDynSIRSStock (
    susceptibleStock,
    infectiousStock,
    recoveredStock,

    infectionRateFlow,
    recoveryRateFlow
  ) where

import SysDynSIRS.SysDynSIRSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
timeDelta :: Double -> SF () Double
timeDelta initDt = proc _ ->
    do
        t <- time -< 0.0
        preT <- iPre initDt -< t
        returnA -< t - preT

filterRateFlowMessageValue :: Double -> (AgentMessage SysDynSIRSMsg) -> Double
filterRateFlowMessageValue initValue (_, Flow flowValue) = flowValue
filterRateFlowMessageValue initValue _ = initValue

filterRateStockMessageValue :: Double -> (AgentMessage SysDynSIRSMsg) -> Double
filterRateStockMessageValue initValue (_, Stock stockValue) = stockValue
filterRateStockMessageValue initValue _ = initValue

susceptibleStock :: SysDynSIRSBehaviour
susceptibleStock = proc ain ->
    do
        let ao = agentOutFromIn ain

        dt <- timeDelta 0.0 -< ()

        let infectiousRate = onMessageFrom infectionRateFlowId ain filterRateFlowMessageValue 0.0
        
        let outFlow = infectiousRate * dt
        let newStockValue = aoState ao - outFlow

        let ao' = setDomainState ao newStockValue

        let ao'' = sendMessage ao' (infectionRateFlowId, Stock newStockValue)

        returnA -< ao''

infectionRateFlow :: SysDynSIRSBehaviour
infectionRateFlow = proc ain ->
    do
        let ao = agentOutFromIn ain

        let susceptible = onMessageFrom susceptibleStockId ain filterRateStockMessageValue 0.0 
        let infectious = onMessageFrom infectiousStockId ain filterRateStockMessageValue 0.0 

        let flowValue = (infectious * contactRate * susceptible) / (totalPopulation * infectivity)
        
        let ao' = sendMessage ao (susceptibleStockId, Flow flowValue)
        let ao'' = sendMessage ao' (infectiousStockId, Flow flowValue)

        returnA -< ao''

infectiousStock :: SysDynSIRSBehaviour
infectiousStock = proc ain ->
    do
        let ao = agentOutFromIn ain

        dt <- timeDelta 0.0 -< ()

        let infectionRate = onMessageFrom infectionRateFlowId ain filterRateFlowMessageValue 0.0
        let recoveryRate = onMessageFrom recoveryRateFlowId ain filterRateFlowMessageValue 0.0

        let inflow = infectionRate * dt
        let outFlow = recoveryRate * dt

        let newStockValue = aoState ao + (inflow - outFlow)

        let ao0 = setDomainState ao newStockValue

        let ao1 = sendMessage ao0 (infectionRateFlowId, Stock newStockValue)
        let ao2 = sendMessage ao1 (recoveryRateFlowId, Stock newStockValue)
        
        returnA -< ao2

recoveryRateFlow :: SysDynSIRSBehaviour
recoveryRateFlow = proc ain ->
    do
        let ao = agentOutFromIn ain

        let infectious = onMessageFrom infectiousStockId ain filterRateStockMessageValue 0.0 

        let flowValue = infectious / avgIllnessDuration
        
        let ao' = sendMessage ao (infectiousStockId, Flow flowValue)
        let ao'' = sendMessage ao' (recoveredStockId, Flow flowValue)

        returnA -< ao''

recoveredStock :: SysDynSIRSBehaviour
recoveredStock = proc ain ->
    do
        let ao = agentOutFromIn ain

        dt <- timeDelta 0.0 -< ()

        let recoveryRate = onMessageFrom recoveryRateFlowId ain filterRateFlowMessageValue 0.0

        let inflow = recoveryRate * dt
        let newStockValue = aoState ao + inflow

        let ao' = setDomainState ao newStockValue

        returnA -< ao'
------------------------------------------------------------------------------------------------------------------------