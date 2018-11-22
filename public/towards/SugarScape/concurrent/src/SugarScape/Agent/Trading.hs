{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Trading
  ( agentTrade
  , handleTradingOffer
  ) where

import Data.Maybe

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.MonadicStreamFunction

import SugarScape.Agent.Common
import SugarScape.Agent.Utils 
import SugarScape.Core.Common
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Random
import SugarScape.Core.Scenario

agentTrade :: RandomGen g
           => SugarScapeScenario               -- parameters of the current sugarscape scenario
           -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
           -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
agentTrade params cont
  | not $ spTradingEnabled params = cont
  | otherwise = tradingRound cont []

tradingRound :: RandomGen g
             => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
             -> [TradeInfo]
             -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
tradingRound cont tradeInfos = do
  myCoord <- agentProperty sugAgCoord
  -- (re-)fetch neighbours from environment, to get up-to-date information
  ns    <- envRun $ neighbours myCoord False
  myMrs <- mrsM

  -- filter out unoccupied sites and traders with same MRS (VERY unlikely with floating point)
  let potentialTraders = mapMaybe (sugEnvSiteOccupier . snd) $ filter (\(_, ss) -> 
                          case sugEnvSiteOccupier ss of 
                            Nothing  -> False
                            Just occ -> sugEnvOccMRS occ /= myMrs) ns

  if null potentialTraders
    then cont -- NOTE: no need to put tradeInfos into observable, because when no potential traders tradeInfos is guaranteed to be null
    else do
      potentialTraders' <- randLift $ fisherYatesShuffleM potentialTraders
      tradeWith cont tradeInfos False potentialTraders'

tradeWith :: RandomGen g
          => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
          -> [TradeInfo]
          -> Bool
          -> [SugEnvSiteOccupier]
          -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
tradeWith cont tradeInfos tradeOccured []       -- iterated through all potential traders => trading round has finished
  | tradeOccured = tradingRound cont tradeInfos -- if we have traded with at least one agent, try another round
  | otherwise    = do
    (ao, mhdl) <- cont
    -- NOTE: at this point we add the trades to the observable output because finished with trading in this time-step
    let ao' = observableTrades tradeInfos ao
    return (ao', mhdl)

tradeWith cont tradeInfos tradeOccured (trader : ts) = do -- trade with next one
  myState <- get

  let myMrsBefore     = mrsState myState     -- agents mrs BEFORE trade
      traderMrsBefore = sugEnvOccMRS trader  -- trade-partners MRS BEFORE trade
      
      (price, sugEx, spiEx) = computeExchange myMrsBefore traderMrsBefore   -- amount of sugar / spice to trade
      
      myWfBefore = agentWelfareState myState                    -- agents welfare BEFORE trade
      myWfAfter  = agentWelfareChangeState myState sugEx spiEx  -- agents welfare AFTER trade

      myMrsAfter = mrsStateChange myState sugEx spiEx           -- agents mrs AFTER trade

  -- NOTE: can't check crossover yet because don't have access to other traders internal state
  -- the crossover will be checked by the other trader and in case of a crossover, the 
  -- trade will be refused. To compute the crossover we need to send the MRS after the
  -- trade as well.

  -- NOTE: check if it makes this agent better off: does it increase the agents welfare?
  if myWfAfter <= myWfBefore
    then tradeWith cont tradeInfos tradeOccured ts -- not better off, continue with next trader
    else do
      let evtHandler = tradingHandler cont tradeInfos tradeOccured ts (price, sugEx, spiEx) 
      ao <- agentObservableM
      sendEventTo (sugEnvOccId trader) (TradingOffer myMrsBefore myMrsAfter)
      return (ao, Just evtHandler)

tradingHandler :: RandomGen g
               => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
               -> [TradeInfo]
               -> Bool
               -> [SugEnvSiteOccupier]
               -> (Double, Double, Double)
               -> EventHandler g
tradingHandler cont0 tradeInfos tradeOccured traders (price, sugEx, spiEx) = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (traderId, TradingReply reply)) -> 
            arrM (uncurry $ handleTradingReply cont0) -< (traderId, reply)
          _ -> do
            aid <- constM myId -< ()
            returnA -< error $ "Agent " ++ show aid ++ ": received unexpected event " ++ show evt ++ " during active Trading, terminating simulation!")
  where
    handleTradingReply :: RandomGen g
                       => AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
                       -> AgentId
                       -> TradingReply
                       -> AgentLocalMonad g (SugAgentOut g, Maybe (EventHandler g))
    handleTradingReply cont _ (RefuseTrade _) =  
      -- the sender refuses the trading-offer, continue with the next trader
      tradeWith cont tradeInfos tradeOccured traders
    handleTradingReply cont traderId AcceptTrade = do -- the sender accepts the trading-offer
      -- NOTE: at this point the trade-partner agent is better off as well, MRS won't cross over and the other agent has already transacted
      let tradeInfos' = TradeInfo price sugEx spiEx traderId : tradeInfos

      transactTradeWealth sugEx spiEx

      -- continue with next trader
      tradeWith cont tradeInfos' True traders

handleTradingOffer :: RandomGen g
                   => AgentId
                   -> Double
                   -> Double
                   -> AgentLocalMonad g (SugAgentOut g)
handleTradingOffer traderId traderMrsBefore traderMrsAfter = do
  myState <- get

  let myMrsBefore  = mrsState myState -- agents mrs BEFORE trade
      -- amount of sugar / spice to trade
      (_, sugEx, spiEx) = computeExchange myMrsBefore traderMrsBefore   
      
      myWfBefore     = agentWelfareState myState                    -- agents welfare BEFORE trade
      myWfAfter      = agentWelfareChangeState myState sugEx spiEx  -- agents welfare AFTER trade
      myMrsAfter     = mrsStateChange myState sugEx spiEx           -- agents mrs AFTER trade

  if myWfAfter <= myWfBefore
    -- not better off, turn offer down
    then sendEventTo traderId (TradingReply $ RefuseTrade NoWelfareIncrease) >> agentObservableM
    else
      if mrsCrossover myMrsBefore traderMrsBefore myMrsAfter traderMrsAfter
        -- MRS cross-over, turn offer down
        then sendEventTo traderId (TradingReply $ RefuseTrade MRSCrossover) >> agentObservableM
        else do
          -- all good, transact and accept offer
          transactTradeWealth sugEx spiEx
          sendEventTo traderId (TradingReply AcceptTrade)
          agentObservableM 

mrsCrossover :: Double
             -> Double
             -> Double
             -> Double
             -> Bool
mrsCrossover mrs1Pre mrs2Pre mrs1Post mrs2Post
  = compare mrs1Pre mrs2Pre /= compare mrs1Post mrs2Post

computeExchange :: Double
                -> Double
                -> (Double, Double, Double)
computeExchange myMrs otherMrs 
    | myMrs > otherMrs = (price, sugEx, -spiEx) -- spice flows from agent with higher mrs (myMrs) to agent with lower (otherMrs) => subtract spice from this agent and add sugar 
    | otherwise        = (price, -sugEx, spiEx) -- sugar flows from agent with lower mrs (myMrs) to agent with higher (otherMrs) => subtract sugar for this agent and add spice
  where
    (price, sugEx, spiEx) = exchangeRates myMrs otherMrs

exchangeRates :: Double
              -> Double
              -> (Double, Double, Double)
exchangeRates myMrs otherMrs 
    | price > 1 = (price, 1, price)     -- trading p units of spice for 1 unit of sugar
    | otherwise = (price, 1 / price, 1) -- trading 1/p units of sugar for 1 unit of spice
  where
    price = sqrt (myMrs * otherMrs) -- price is the geometric mean

transactTradeWealth :: RandomGen g
                    => Double
                    -> Double
                    -> AgentLocalMonad g ()
transactTradeWealth sugEx spiEx = do
  -- NOTE: negative values shouldn't happen due to welfare increase / MRS crossover restrictions
  -- but for security reasons make sure that we cap at 0 and cant go below
  updateAgentState (\s -> s { sugAgSugarLevel = max (sugAgSugarLevel s + sugEx) 0
                            , sugAgSpiceLevel = max (sugAgSpiceLevel s + spiEx) 0 })

  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteOccupied