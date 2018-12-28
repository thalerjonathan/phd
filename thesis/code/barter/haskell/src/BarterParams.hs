module BarterParams
  ( BarterParams  -- note only export data-constructor, not the accessors

  , mkBarterParams
  
  , getNumGoods
  , getConsumeArray
  , getMaxRetries
  , getTradeOrder
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