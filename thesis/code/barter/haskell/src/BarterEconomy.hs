module BarterEconomy
  ( BarterEconomyState
  , init
  ) where

data BarterEconomyState = BarterEconomyState
  { traders      :: ![TradeAgentState]

  , meanPrices   :: ![Double]
  , avgPrices    :: ![Double]
  , avgRelPrices :: ![Double]
  , avgScore     :: ![Double]
  , numProducers :: ![Int]
  }

init :: BarterParams -> BarterEconomyState
init params = BarterEconomyState 
    { traders      = ts
    , meanPrices   = []
    , avgPrices    = replicate n 0
    , avgRelPrices = replicate n 0
    , avgScore     = replicate n 0
    }
  where
    n  = getNumGoods params

    np = getNumProducers
    ts = createTraders  

createTraders :: [Int] -> [TradeAgentState]
createTraders ns = []