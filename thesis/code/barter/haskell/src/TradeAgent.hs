module TradeAgent
  ( TradeAgentState -- don't export internals of state

  , mkTradeAgent
  , step
  ) where

import Control.Monad.Random

import BarterParams
import Utils

-- TODO: need a generalisation of the barter-strategy
-- TODO: need a generalisation of the improvement-strategy

data TradeAgentState = TradeAgentState 
  { agentIndex    :: !Int

  , produceGood   :: !Int
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
             -> Int 
             -> [Double]
             -> TradeAgentState
mkTradeAgent params ai pg pr = produce $ TradeAgentState
    { agentIndex    = ai

    , produceGood   = pg
    , score         = 0
    , produceAmount = prodAm

    , price         = pr
    , demand        = replicate n 0
    , exchangeFor   = replicate n 0
    , inventory     = replicate n 0
    , consume       = conArr
    }
  where
    n = getNumGoods params

    conArr = getConsumeArray params
    prodAm = conArr !! pg * fromIntegral (length conArr);

produce :: TradeAgentState -> TradeAgentState
produce s = setDemandAndSupply $ s { inventory = inventory'' }
  where
    n           = length $ price s
    inventory'  = replicate n 0
    inventory'' = replaceElem (produceGood s) (produceAmount s) inventory'
    -- TODO: check invariants

setDemandAndSupply :: TradeAgentState -> TradeAgentState
setDemandAndSupply s 
    = s { demand      = demand''
        , exchangeFor = exchangeFor'' }
  where
    l            = lambda s
    priceOfGood  = price s !! produceGood s

    demand'      = map (* l) (consume s) 
    exchangeFor' = zipWith (\p d -> p * d / priceOfGood) (price s) (demand s)

    demand''      = replaceElem (produceGood s) 0 demand'
    exchangeFor'' = replaceElem (produceGood s) 0 exchangeFor'

{-
  How much value agent wants to consume divided by how much value it has
  produced (according to its own prices).
-}
lambda :: TradeAgentState -> Double
lambda s 
    | incomeConstraint > consumeValue = 1
    | otherwise = incomeConstraint / consumeValue -- See section 3.2, eq. 4, \equiv \lambda^*
  where
    -- See section 3.2, eq. 2, incomeConstraint \equiv M^o
    incomeConstraint = price s !! produceGood s * inventory s !! produceGood s

    -- Compute denominator of eq. 4 from section 3.2.
    -- See sec. 3.2, eq. 4
    -- consumeValue = foldr (\(p, c) acc -> acc + p * c) 0 (zip (price s) (consume s))
    -- REVERT-BUGS
    consumeValue = foldr (\(p, c) acc -> acc + p * c) 0 (zip (price s) (inventory s))

-- start trading
step :: RandomGen g
     => BarterParams
     -> TradeAgentState 
     -> [TradeAgentState]
     -> Rand g (Maybe (TradeAgentState, TradeAgentState))
step params s0 ss = iterateGoodPermuation tradeGoodPermutation s0
  where
    maxRetries           = getMaxRetries params
    tradeGoodPermutation = getTradeOrder params

    iterateGoodPermuation :: RandomGen g
                          => [Int]
                          -> TradeAgentState
                          -> Rand g (Maybe (TradeAgentState, TradeAgentState))
    iterateGoodPermuation [] _ = return Nothing -- TODO return updated 
    iterateGoodPermuation (wantGood:gs) s
      | demand s !! wantGood == 0 = iterateGoodPermuation gs s
      | otherwise = findTrades maxRetries s wantGood producers
        where
          producers = filter (\s' -> produceGood s' == wantGood) ss 

    findTrades :: RandomGen g
               => Int
               -> TradeAgentState
               -> Int
               -> [TradeAgentState]
               -> Rand g (Maybe (TradeAgentState, TradeAgentState))
    findTrades 0 _ _ _ = return Nothing
    findTrades n s wantGood producers = do
      responder <- randomElem producers
      
      if not $ acceptOffer responder (produceGood s) (demand s !! wantGood) (exchangeFor s !! wantGood)
        then findTrades (n-1) s wantGood producers
        else return $ Just $ transactTrade wantGood s responder

    transactTrade :: Int 
                  -> TradeAgentState
                  -> TradeAgentState
                  -> (TradeAgentState, TradeAgentState)
    transactTrade wantGood s responder 
        = (s, responder)
      where
        amounts = [demand s !! wantGood, exchangeFor s !! wantGood]
        pg      = produceGood s

        (adjustedDemand, adustedExchange)   = adjustAmounts s wantGood 0 pg 1 amounts
        (adjustedDemand', adustedExchange') = adjustAmounts responder pg 1 wantGood 0 [adjustedDemand, adustedExchange]

        exchangeGoods wantGood adjustedDemand' pg adustedExchange' s
        --exchangeGoods pg adustedExchange' wantGood adjustedDemand' responder

exchangeGoods :: Int
              -> Double
              -> Int 
              -> Double
              -> TradeAgentState
              -> TradeAgentState
exchangeGoods inGood inAmount outGood outAmount s0
    = s4 { inventory   = inv
        , demand      = dem
        , exchangeFor = exc }
  where
    s1 = changeInventory inGood inAmount s0
    s2 = changeInventory outGood (-outAmount) s1

    s3 = changeDemand inGood (-inAmount) s2
    
    -- exchangeFor[inGood] = validateAmount(price[inGood] * demand[inGood] / price[outGood]);
    s4 = setExchange inGood (validateAmount $ price s3 !! inGood * demand s3 !! inGood / price s3 !! outGood) s3
    -- s4 = changeExchange inGood (-outAmount) s3 -- REVERT-BUGS

    inv = map validateAmount (inventory s4)
    dem = map validateAmount (demand s4)
    exc = map validateAmount (exchangeFor s4)

    setStateArrayValue :: Int
                      -> Double
                      -> (TradeAgentState -> [Double])
                      -> ([Double] -> TradeAgentState)
                      -> TradeAgentState
                      -> TradeAgentState
    setStateArrayValue idx v proj updt s 
        = updt arr' s
      where
        arr  = proj s
        arr' = replaceElem idx v arr

    setExchange idx v s
      = setStateArrayValue idx v exchangeFor (\e -> s { exchangeFor = e })

    updateStateArrayValue :: Int
                          -> Double
                          -> (TradeAgentState -> [Double])
                          -> ([Double] -> TradeAgentState)
                          -> TradeAgentState
                          -> TradeAgentState
    updateStateArrayValue idx v proj updt s 
        = updt arr' s
      where
        arr    = proj s
        arrVal = arr !! idx
        arr'   = replaceElem idx (arrVal + v) arr

    changeInventory idx v s 
        = updateStateArrayValue idx v inventory (\i -> s { inventory = i })
    changeDemand idx v s 
        = updateStateArrayValue idx v demand (\d -> s { demand = d })
    changeExchange idx v s 
        = updateStateArrayValue idx v exchangeFor (\e -> s { exchangeFor = e })

adjustAmounts :: TradeAgentState
              -> Int
              -> Int
              -> Int
              -> Int
              -> [Double]
              -> (Double, Double)
adjustAmounts s inGood inIdx outGood outIdx amounts 
    = (outAmount3, inAmount3)
  where
    outAmount0 = amounts !! outIdx
    inAmount0  = amounts !! inIdx

    haveAmount = inventory s !! outGood
    (outAmount1, inAmount1) 
      = if outAmount0 > haveAmount
          -- we are offering more than we have, so lets offer only the
          -- amount that we have and request less by the same factor
          then (haveAmount, inAmount0 * (haveAmount / outAmount0)) -- inAmount *= haveAmount / outAmount; outAmount = haveAmount;
          else (outAmount0, inAmount0)

    -- shouldn't be the case for offerer!
    demandAmount = demand s !! inGood
    (outAmount2, inAmount2)
      = if inAmount1 > demandAmount
          -- lets take only as much as we want and give less by the same factor
          then (outAmount0 * (demandAmount / inAmount1), demandAmount) -- outAmount *= demandAmount / inAmount; inAmount = demandAmount;
          else (outAmount1, inAmount1)

    -- REVERT-BUGS is relevant if don't set demand and supply after getting next generation
    (outAmount3, inAmount3)
      = if exchangeFor s !! inGood < outAmount2
          -- we don't agree to give as much as requested, so lets give what
          -- we can and take less by the same factor
          then (exchangeFor s !! inGood, exchangeFor s !! inGood * inAmount2) -- inAmount *= exchangeFor[inGood] / outAmount; outAmount = exchangeFor[inGood];
          else (outAmount2, inAmount2)

          {-}
    // to ensure that trade conditions have not been changed
    assert outAmount == 0.0 ||
      Math.abs(amounts[inIdx] / amounts[outIdx] - inAmount / outAmount) < 0.00001
          : amounts[inIdx] / amounts[outIdx] + " vs " + inAmount / outAmount;

    amounts[inIdx] = inAmount;
    amounts[outIdx] = outAmount;
    -}

acceptOffer :: TradeAgentState 
            -> Int
            -> Double
            -> Double
            -> Bool 
acceptOffer s offerGood offerAmount wantAmount
    = inventory s !! pg > 0
    && not (demand s !! offerGood == 0 
           || exchangeFor s !! offerGood == 0
           || price s !! offerGood * offerAmount < price s !! pg * wantAmount)
  where
    pg = produceGood s

_checkInvariants :: TradeAgentState -> Bool
_checkInvariants s 
    =  n /= length (price s)
    || n /= length (demand s)
    || n /= length (exchangeFor s)
    || n /= length (inventory s)
    || pg < 0
    || pg >= n
    || produceAmount s /= consume s !! pg * fromIntegral n
    || score s < 0
  where
    n = length $ consume s
    pg = produceGood s