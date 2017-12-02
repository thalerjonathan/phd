{-# LANGUAGE Arrows #-}
module StockFlow 
  (
    susceptibleStock
  , infectiousStock
  , recoveredStock

  , infectionRateFlow
  , recoveryRateFlow
  ) where

import FRP.Chimera
import FRP.Yampa

import Model

------------------------------------------------------------------------------------------------------------------------
-- STOCKS
------------------------------------------------------------------------------------------------------------------------
susceptibleStock :: Stock
susceptibleStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (-infectionRate)
  
  let ao = agentOutObs stockValue
  let ao' = stockOutTo stockValue infectionRateFlowId ao

  returnA -< ao'

infectiousStock :: Stock
infectiousStock initValue = proc ain -> do
  let infectionRate = flowInFrom infectionRateFlowId ain
  let recoveryRate = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< (infectionRate - recoveryRate)
  
  let ao = agentOutObs stockValue
  let ao' = stockOutTo stockValue infectionRateFlowId ao 
  let ao'' = stockOutTo stockValue recoveryRateFlowId ao'
  
  returnA -< ao''

recoveredStock :: Stock
recoveredStock initValue = proc ain -> do
  let recoveryRate = flowInFrom recoveryRateFlowId ain

  stockValue <- (initValue+) ^<< integral -< recoveryRate
  
  let ao = agentOutObs stockValue

  returnA -< ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- FLOWS
------------------------------------------------------------------------------------------------------------------------
infectionRateFlow :: Flow
infectionRateFlow = proc ain -> do
  let susceptible = stockInFrom susceptibleStockId ain 
  let infectious = stockInFrom infectiousStockId ain

  let flowValue = (infectious * contactRate * susceptible * infectivity) / totalPopulation
  
  let ao' = flowOutTo flowValue susceptibleStockId agentOut
  let ao'' = flowOutTo flowValue infectiousStockId ao'

  returnA -< ao''

recoveryRateFlow :: Flow
recoveryRateFlow = proc ain -> do
  let infectious = stockInFrom infectiousStockId ain

  let flowValue = infectious / avgIllnessDuration

  let ao' = flowOutTo flowValue infectiousStockId agentOut
  let ao'' = flowOutTo flowValue recoveredStockId ao'

  returnA -< ao''
------------------------------------------------------------------------------------------------------------------------