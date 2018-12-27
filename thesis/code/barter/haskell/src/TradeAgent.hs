module TradeAgent
  ( TradeAgentState -- don't export internals of state
  , mkTradeAgent
  ) where

import BarterParams

-- TODO: need a generalisation of the barter-strategy
-- TODO: need a generalisation of the improvement-strategy

data TradeAgentState = TradeAgentState 
  { produceGood   :: !Int
  , score         :: !Double
  , produceAmount :: !Double

  , price         :: ![Double]
  , demand        :: ![Double]
  , exchangeFor   :: ![Double]
  , inventory     :: ![Double]
  , consume       :: ![Double]
  }

mkTradeAgent :: BarterParams
             -> Int 
             -> [Double]
             -> TradeAgentState
mkTradeAgent params pg pr = TradeAgentState
    { produceGood   = pg
    , score         = 0
    , produceAmount = 0

    , price         = pr
    , demand        = replicate n 0
    , exchangeFor   = replicate n 0
    , inventory     = replicate n 0
    , consume       = []
    }
  where
    n = getNumGoods params