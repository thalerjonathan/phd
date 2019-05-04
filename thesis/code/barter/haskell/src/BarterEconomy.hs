module BarterEconomy
  ( -- BarterEconomyState
    start
  ) where

import Control.Monad.Random

import BarterParams
import TradeAgent
import Utils

-- data BarterEconomyState = BarterEconomyState
--   { traders      :: ![TradeAgentState]

--   , meanPrices   :: ![Double]
--   , avgPrices    :: ![Double]
--   , avgRelPrices :: ![Double]
--   , avgScores    :: ![Double]
--   , numProducers :: ![Int]
--   }

start :: RandomGen g => Rand g ()
start = do
  let params = mkBarterParams
      
  nps <- getNumProducers params

  ts0  <- createTraders params True nps  
  ts0' <- scheduleAgents params ts0

  let (_, _, _, _, totalAvgScore) = calcAvgScore params [] ts0'
  let commonPriceScore = totalAvgScore;

  ts1  <- createTraders params False nps 
  ts1' <- scheduleAgents params ts1

  let (_, _, _, _, _) = calcAvgScore params [] ts1'

  -- TODO now regular scheduling happens

  return ()

-- MOST HORRIBLE FUNCTION DUE TO MUTABLE LIST PROGRAMMING. NEED TO FIND A BETTER
-- MORE FUNCTIONAL REPRESENTATION
calcAvgScore :: BarterParams
             -> [Double]
             -> [[TradeAgentState]]
             -> ([Double], [Double], [Double], [Double], Double)
calcAvgScore params meanPrices ass
    = (meanPrices'', avgPrice', avgRelPrice, avgScore', totalAvgScore')
  where
    as = concat ass
    ng = getNumGoods params
    ac = fromIntegral $ getTotalAgents params -- cant we compute this from as length?

    avgPrice = foldr avgPriceAux (replicate ng 0) as
    avgScore = foldr avgScoreAux (replicate ng 0) as

    avgPrice'   = map (/ac) avgPrice
    equiPrices  = getEquiPrice params
    avgRelPrice = zipWith (\p ep -> (p - ep) / ep) avgPrice' equiPrices
    avgScore'   = zipWith (\s as' -> s / fromIntegral (length as')) avgScore ass
    totalAvgScore = sum avgScore'
    totalAvgScore' = totalAvgScore / fromIntegral ng

    meanPrices' = meanPrices ++ [meanPrices !! getStdDevGood params]
    meanPrices'' = if length meanPrices' > getReproducePeriod params
                    then tail meanPrices'
                    else meanPrices'

    avgScoreAux :: TradeAgentState
                -> [Double]
                -> [Double]
    avgScoreAux a ss 
        = updateElem idx (+sc) ss
      where
        sc  = getScore a
        idx = getProduceGood a

    avgPriceAux :: TradeAgentState
                -> [Double]
                -> [Double]
    avgPriceAux a ps 
        = zipWith (\agentPrice p -> p + (agentPrice / aps !! pug)) aps ps
      where
        aps = getPrices a
        pug = getPriceUnitGood params

scheduleAgents :: RandomGen g
               => BarterParams 
               -> [[TradeAgentState]]
               -> Rand g [[TradeAgentState]]
scheduleAgents _prams as0 = do
    asShuff <- randomShuffleM as0
    let asFlat = concat asShuff
    as' <- foldM stepAgent asFlat asFlat
    -- TODO: sort into produce goods
    return [as']
  where
    stepAgent :: RandomGen g
              => [TradeAgentState]
              -> TradeAgentState
              -> Rand g [TradeAgentState]
    stepAgent as _a = return as -- do
      --step params a as

createTraders :: RandomGen g
              => BarterParams               -- ^ simulation params
              -> Bool                       -- ^ common price
              -> [Int]                      -- ^ num producers
              -> Rand g [[TradeAgentState]] -- ^ agents for each produce good
createTraders params commonPrice nps 
    = zipWithM createTradersGood [0..] nps
  where
    createTradersGood :: RandomGen g
                      => Int                      -- ^ produce good, 
                      -> Int                      -- ^number of producers for that good
                      -> Rand g [TradeAgentState] -- ^ agents for that produce good
    createTradersGood pg n = do
      prices <- if commonPrice 
                  then return $ getEquiPrice params 
                  else getPriceVector params pg

      return $ replicate n (mkTradeAgent 0 pg prices params)

getNumProducers :: RandomGen g
                => BarterParams 
                -> Rand g [Int]
getNumProducers params 
    | not $ isVarySupply params = return $ replicate ng apg
    -- divide producers among goods unequally (prefers first goods)
    | otherwise = do
      let numProd   = replicate ng 10 -- // at least 10 for each good
          maxAgents = ng * (apg - 10);

      nps <- take ng <$> getRandomRs (0, maxAgents) -- in Haskell the range is inclusive

      let numProd'   = zipWith (\n r -> n+r) numProd nps
          maxAgents' = maxAgents - (sum numProd') - 10

          numProd'' = updateElem (ng - 1) (+maxAgents') numProd'
      
      return numProd''
  where
    ng  = getNumGoods params
    apg = getAgentsPerGood params

getPriceVector :: RandomGen g
               => BarterParams    -- ^ simulation params
               -> Int             -- ^ produce good
               -> Rand g [Double] -- ^ price vector
getPriceVector params pg = do
    ps <- if isCheckEfficiency params 
            then return $ replicate n 1
            else if isEquiInitialPrices params
              then return $ getEquiPrice params
              else take n <$> getRandoms

    return $ updateElem pg (*pgpf) ps
  where
    n    = getNumGoods params
    pgpf = getProduceGoodPriceFactor params

-- mkBarterEconomyState :: BarterParams -> BarterEconomyState
-- mkBarterEconomyState params = BarterEconomyState 
--     { traders      = []
--     , meanPrices   = []
--     , avgPrices    = replicate n 0
--     , avgRelPrices = replicate n 0
--     , avgScores    = replicate n 0
--     , numProducers = []
--     }
--   where
--     n = getNumGoods params
