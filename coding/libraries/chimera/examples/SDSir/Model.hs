module Model 
  (
    totalPopulation
  , infectivity
  , contactRate
  , avgIllnessDuration

  , susceptibleStockId
  , infectiousStockId
  , recoveredStockId
  , infectionRateFlowId
  , recoveryRateFlowId
  ) where

import FRP.FrABS

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
-- Hard-coded ids for stocks & flows interaction (sending messages)
susceptibleStockId :: StockId
susceptibleStockId = 0

infectiousStockId :: StockId
infectiousStockId = 1

recoveredStockId :: StockId
recoveredStockId = 2

infectionRateFlowId :: FlowId
infectionRateFlowId = 3

recoveryRateFlowId :: FlowId
recoveryRateFlowId = 4
------------------------------------------------------------------------------------------------------------------------