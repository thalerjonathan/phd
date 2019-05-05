module BarterEconomy
  ( -- BarterEconomyState
    start
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Maybe

import BarterParams
import TradeAgent
import Utils

import Debug.Trace

-- TODO: replace RandomGen g and Rand by MonadRandom

start :: RandomGen g => Rand g [[Double]]
start = do
  let params = mkBarterParams

  nps <- trace (show params) getNumProducers params

  ts0  <- createTraders params True nps  
  ts0' <- scheduleAgents params ts0

  let (_, _, _, _, totalAvgScore) = calcAvgScore params [] ts0'
  let _commonPriceScore = totalAvgScore;

  ts1  <- createTraders params False nps 
  ts1' <- scheduleAgents params ts1

  let (meanPrices, _, _, _,_) = calcAvgScore params [] ts1'

  simulation 1 params ts1' meanPrices

simulation :: RandomGen g
           => Int                 -- ^ Period
           -> BarterParams        -- ^ Parameters
           -> [[TradeAgentState]] -- ^ Agents
           -> [Double]            -- ^ Mean prices
           -> Rand g [[Double]]   -- ^ mean prices per period
simulation p params ass0 meanPrices = do
    ass'  <- period ass0
    ass'' <- reproducePeriod ass'

    let (meanPrices', _, _, _,_) = calcAvgScore params meanPrices ass''

    mpss <- simulation (p+1) params ass'' meanPrices'
    return (meanPrices' : mpss)
  where
    period :: RandomGen g
           => [[TradeAgentState]] 
           -> Rand g [[TradeAgentState]]
    period ass = do
      -- every agent produces after each period
      let ass' = map (map produce) ass
      -- reschedule agents
      scheduleAgents params ass'

    -- after getReproducePeriod params there is a reproduction period AFTER all agents have run
    reproducePeriod :: RandomGen g 
                    => [[TradeAgentState]] 
                    -> Rand g [[TradeAgentState]]
    reproducePeriod ass 
      -- no reproduction yet
      | p `mod` getReproducePeriod params /= 0 = return ass 
      -- reproduction period
      | otherwise = do
        let (_meanPrices, _, _, _,_) = calcAvgScore params meanPrices ass
        --ass' <- mapM (map resetScore <$> getNextGeneration params) ass
        mapM (\as -> map resetScore <$> getNextGeneration params as) ass
  -- if (params.isVarySupply()) {
  --   producerShift();
  -- }


-- An implementation of the replacement strategy that corresponds
--  to the algorithm described in Gintis' paper. Picks two random agents
--  and replaces the worse one with the better.
getNextGeneration :: RandomGen g
                  => BarterParams
                  -> [TradeAgentState] 
                  -> Rand g [TradeAgentState]
getNextGeneration params as0 = do
  let n  = length as0
      i  = round (fromIntegral n * getReplacementRate params)
      i' = max i (1 :: Int)

  foldM (\as _ -> do
    j <- getRandomR (0, n-1)
    k <- getRandomR (0, n-1)

    let aj = as !! j
        ak = as !! k

    (aj', ak') <- improveAgents params aj ak

    let as' = replaceElem k ak' (replaceElem j aj' as)

    return as') as0 [0..i' - 1]

improveAgents :: RandomGen g
              => BarterParams
              -> TradeAgentState
              -> TradeAgentState
              -> Rand g (TradeAgentState, TradeAgentState)
improveAgents params aj ak = 
  if getScore aj > getScore ak
    then do
      ak' <- improve params ak aj
      return (aj, ak')
    else do
      aj' <- improve params aj ak
      return (aj', ak)

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
    ac = fromIntegral $ getTotalAgents params -- cant use length as?

    avgPrice = foldr avgPriceAux (replicate ng 0) as
    avgScore = foldr avgScoreAux (replicate ng 0) as

    avgPrice'   = map (/ac) avgPrice
    equiPrices  = getEquiPrice params
    avgRelPrice = zipWith (\p ep -> (p - ep) / ep) avgPrice' equiPrices
    avgScore'   = zipWith (\s as' -> s / fromIntegral (length as')) avgScore ass

    totalAvgScore  = sum avgScore'
    totalAvgScore' = totalAvgScore / fromIntegral ng

    meanPrices'  = meanPrices ++ [avgPrice' !! getStdDevGood params]
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
        = zipWith (\agentPrice p -> p + (agentPrice / (aps !! pug))) aps ps
      where
        aps = getPrices a
        pug = getPriceUnitGood params

scheduleAgents :: RandomGen g
               => BarterParams 
               -> [[TradeAgentState]]
               -> Rand g [[TradeAgentState]]
scheduleAgents params ass0 = do
    perm <- randomPermM (getNumGoods params)
    let asFlat = concat ass0
    foldM (stepAgent perm) ass0 asFlat
  where
    stepAgent :: RandomGen g
              => [Int]
              -> [[TradeAgentState]]
              -> TradeAgentState
              -> Rand g [[TradeAgentState]]
    stepAgent perm ass a = do
      tradeMay <- step params perm a ass
      case tradeMay of
        Nothing -> return ass

        (Just (a', other)) -> do
          -- we can assume that the elements will be found
          let (aRow, aCol) = fromJust $ findElem a' ass
              (oRow, oCol) = fromJust $ findElem other ass

          let ass'  = updateElem aRow (replaceElem aCol a') ass
              ass'' = updateElem oRow (replaceElem oCol other) ass'

          return ass''

createTraders :: RandomGen g
              => BarterParams               -- ^ simulation params
              -> Bool                       -- ^ common price
              -> [Int]                      -- ^ num producers
              -> Rand g [[TradeAgentState]] -- ^ agents for each produce good
createTraders params commonPrice nps
    = evalStateT (zipWithM createTradersGood [0..] nps) 0
  where
    createTradersGood :: RandomGen g
                      => Int                      -- ^ produce good, 
                      -> Int                      -- ^number of producers for that good
                      -> StateT Int (Rand g) [TradeAgentState] -- ^ agents for that produce good
    createTradersGood pg n = do
      prices <- if commonPrice 
                  then return $ getEquiPrice params 
                  else lift $ getPriceVector params pg

      replicateM n (do
        -- get current agent id
        ai <- get
        -- increment for next
        modify (+1)
        return $ mkTradeAgent ai pg prices params)

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
