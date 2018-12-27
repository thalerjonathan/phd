module BarterParams
  ( BarterParams  -- note only export data-constructor, not the accessors
  , getNumGoods
  ) where

data BarterParams = BarterParams
  {

  }

getNumGoods :: BarterParams -> Int
getNumGoods = undefined