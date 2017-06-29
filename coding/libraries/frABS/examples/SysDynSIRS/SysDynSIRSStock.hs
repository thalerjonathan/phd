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

import Text.Printf
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

        --dt <- timeDelta 0.0 -< ()

        let infectiousRate = onMessageFrom infectionRateFlowId ain filterRateFlowMessageValue 0.0
        
        let outFlow = infectiousRate
        let newStockValue = aoState ao - outFlow

        let ao' = setDomainState ao newStockValue

        let ao'' = sendMessage ao' (infectionRateFlowId, Stock newStockValue)

        returnA -< ao''

infectionRateFlow :: SysDynSIRSBehaviour
infectionRateFlow = proc ain ->
    do
        let ao = agentOutFromIn ain

        dt <- timeDelta 0.0 -< ()

        let susceptible = onMessageFrom susceptibleStockId ain filterRateStockMessageValue 0.0 
        let infectious = onMessageFrom infectiousStockId ain filterRateStockMessageValue 0.0 

        let flowValue = dt * (infectious * contactRate * (susceptible / totalPopulation) * infectivity)
        
        let ao' = sendMessage ao (susceptibleStockId, Flow flowValue)
        let ao'' = sendMessage ao' (infectiousStockId, Flow flowValue)

        returnA -< ao''

infectiousStock :: SysDynSIRSBehaviour
infectiousStock = proc ain ->
    do
        let ao = agentOutFromIn ain

        let infectionRate = onMessageFrom infectionRateFlowId ain filterRateFlowMessageValue 0.0
        let recoveryRate = onMessageFrom recoveryRateFlowId ain filterRateFlowMessageValue 0.0

        let inflow = infectionRate 
        let outFlow = recoveryRate 

        let changeRate = inflow - outFlow

        let newStockValue = aoState ao + changeRate

        {-
        let ao0 = trace ("infectiousStock: value = " 
                    ++ (printf "%.2f" (aoState ao)) ++ ", "
                    ++ "infectionRate = " ++ (printf "%.3f" infectionRate) ++ ", " 
                    ++ "recoveryRate = " ++ (printf "%.3f" recoveryRate) ++ ", " 
                    ++ "changeRate = " ++ (printf "%.3f" changeRate) ++ ", " 
                    ++ "newStockValue = " ++ (printf "%.3f" newStockValue) )
                    (setDomainState ao newStockValue)
        -}
        let ao0 = setDomainState ao newStockValue
        let ao1 = sendMessage ao0 (infectionRateFlowId, Stock newStockValue)
        let ao2 = sendMessage ao1 (recoveryRateFlowId, Stock newStockValue)
        
        returnA -< ao2

recoveryRateFlow :: SysDynSIRSBehaviour
recoveryRateFlow = proc ain ->
    do
        let ao = agentOutFromIn ain

        dt <- timeDelta 0.0 -< ()

        let infectious = onMessageFrom infectiousStockId ain filterRateStockMessageValue 0.0 

        let flowValue = dt * (infectious / avgIllnessDuration)
        
        let ao' = sendMessage ao (infectiousStockId, Flow flowValue)
        let ao'' = sendMessage ao' (recoveredStockId, Flow flowValue)

        returnA -< ao''

recoveredStock :: SysDynSIRSBehaviour
recoveredStock = proc ain ->
    do
        let ao = agentOutFromIn ain
        let recoveryRate = onMessageFrom recoveryRateFlowId ain filterRateFlowMessageValue 0.0

        let inflow = recoveryRate
        let newStockValue = aoState ao + inflow

        let ao' = setDomainState ao newStockValue

        returnA -< ao'
------------------------------------------------------------------------------------------------------------------------