module BarterParams
  ( BarterParams  -- note only export data-constructor, not the accessors

  , mkBarterParams
  
  , getNumGoods
  , getConsumeArray
  , getMaxRetries
  , getTradeOrder
  , getEquiPrice
  , isCheckEfficiency
  , getProduceGoodPriceFactor
  , isEquiInitialPrices
  , getAgentsPerGood
  , isVarySupply
  , getPriceUnitGood
  , getStdDevGood
  , getReproducePeriod
  , getTotalAgents
  ) where

data BarterParams = BarterParams
  {

  }

mkBarterParams :: BarterParams
mkBarterParams = BarterParams {}

getNumGoods :: BarterParams -> Int
getNumGoods = undefined

getConsumeArray :: BarterParams -> [Double]
getConsumeArray = undefined

getMaxRetries :: BarterParams -> Int
getMaxRetries = undefined

getTradeOrder :: BarterParams -> [Int]
getTradeOrder = undefined

getEquiPrice :: BarterParams -> [Double]
getEquiPrice = undefined

isCheckEfficiency :: BarterParams -> Bool
isCheckEfficiency = undefined

getProduceGoodPriceFactor :: BarterParams -> Double
getProduceGoodPriceFactor = undefined

isEquiInitialPrices :: BarterParams -> Bool
isEquiInitialPrices = undefined

getAgentsPerGood :: BarterParams -> Int
getAgentsPerGood = undefined

isVarySupply :: BarterParams -> Bool
isVarySupply = undefined

getPriceUnitGood :: BarterParams -> Int
getPriceUnitGood = undefined

getStdDevGood :: BarterParams -> Int
getStdDevGood = undefined

getReproducePeriod :: BarterParams -> Int
getReproducePeriod = undefined

getTotalAgents :: BarterParams -> Int
getTotalAgents = undefined