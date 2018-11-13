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
import SugarScape.Agent.Interface
import SugarScape.Agent.Utils 
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random

agentTrade :: RandomGen g
           => SugarScapeParams               -- parameters of the current sugarscape scenario
           -> AgentId                        -- the id of the agent 
           -> EventHandler g                 -- global event handler to switch back into after trading has finished
           -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
agentTrade params myId globalHdl
  | not $ spTradingEnabled params = do
    ao <- agentObservableM
    return (ao, Nothing)
  | otherwise = tradingRound myId globalHdl []

tradingRound :: RandomGen g
             => AgentId
             -> EventHandler g
             -> [TradeInfo]
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
tradingRound myId globalHdl tradeInfos = do
  myCoord <- agentProperty sugAgCoord
  -- (re-)fetch neighbours from environment, to get up-to-date information
  ns    <- envLift $ neighboursM myCoord False
  myMrs <- mrsM

  -- filter out unoccupied sites and traders with same MRS (VERY unlikely with floating point)
  let potentialTraders = mapMaybe (sugEnvSiteOccupier . snd) $ filter (\(_, ss) -> 
                          case sugEnvSiteOccupier ss of 
                            Nothing  -> False
                            Just occ -> sugEnvOccMRS occ /= myMrs) ns

  if null potentialTraders
    then do
      -- NOTE: no need to put tradeInfos into observable, because when no potential traders tradeInfos is guaranteed to be null
      ao <- agentObservableM
      return (ao, Just globalHdl)
    else do
      potentialTraders' <- randLift $ fisherYatesShuffleM potentialTraders
      tradeWith myId globalHdl tradeInfos False potentialTraders'

tradeWith :: RandomGen g
          => AgentId
          -> EventHandler g
          -> [TradeInfo]
          -> Bool
          -> [SugEnvSiteOccupier]
          -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
tradeWith myId globalHdl tradeInfos tradeOccured []       -- iterated through all potential traders => trading round has finished
  | tradeOccured = tradingRound myId globalHdl tradeInfos -- if we have traded with at least one agent, try another round
  | otherwise    = do
    ao <- fmap (observableTrades tradeInfos) agentObservableM
    -- NOTE: at this point we add the trades to the observable output because finished with trading in this time-step
    return (ao, Just globalHdl) -- no trading has occured, quit trading and switch back to globalHandler

tradeWith myId globalHdl tradeInfos tradeOccured (trader : ts) = do -- trade with next one
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
    then tradeWith myId globalHdl tradeInfos tradeOccured ts -- not better off, continue with next trader
    else do
      let evtHandler = tradingHandler myId globalHdl tradeInfos tradeOccured ts (price, sugEx, spiEx) 
      ao <- agentObservableM
      return (sendEventTo (sugEnvOccId trader) (TradingOffer myMrsBefore myMrsAfter) ao, Just evtHandler)

tradingHandler :: RandomGen g
               => AgentId
               -> EventHandler g
               -> [TradeInfo]
               -> Bool
               -> [SugEnvSiteOccupier]
               -> (Double, Double, Double)
               -> EventHandler g
tradingHandler myId globalHdl0 tradeInfos tradeOccured traders (price, sugEx, spiEx) = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (traderId, TradingReply reply)) -> 
            arrM (uncurry $ handleTradingReply globalHdl0) -< (traderId, reply)
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Trading, terminating simulation!")
  where
    handleTradingReply :: RandomGen g
                       => EventHandler g
                       -> AgentId
                       -> TradingReply
                       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleTradingReply globalHdl _ (Refuse _) =  
      -- the sender refuses the trading-offer, continue with the next trader
      tradeWith myId globalHdl tradeInfos tradeOccured traders
    handleTradingReply globalHdl traderId Accept = do -- the sender accepts the trading-offer
      -- NOTE: at this point the trade-partner agent is better off as well, MRS won't cross over and the other agent has already transacted
      let tradeInfos' = TradeInfo price sugEx spiEx traderId : tradeInfos

      transactTradeWealth myId sugEx spiEx

      -- continue with next trader
      tradeWith myId globalHdl tradeInfos' True traders

handleTradingOffer :: RandomGen g
                   => AgentId
                   -> AgentId
                   -> Double
                   -> Double
                   -> AgentAction g (SugAgentOut g)
handleTradingOffer myId traderId traderMrsBefore traderMrsAfter = do
  myState <- get

  let myMrsBefore  = mrsState myState -- agents mrs BEFORE trade
      -- amount of sugar / spice to trade
      (_, sugEx, spiEx) = computeExchange myMrsBefore traderMrsBefore   
      
      myWfBefore     = agentWelfareState myState                    -- agents welfare BEFORE trade
      myWfAfter      = agentWelfareChangeState myState sugEx spiEx  -- agents welfare AFTER trade
      myMrsAfter     = mrsStateChange myState sugEx spiEx           -- agents mrs AFTER trade

  if myWfAfter <= myWfBefore
    then do -- not better off, turn offer down
      ao <- agentObservableM
      return (sendEventTo traderId (TradingReply $ Refuse NoWelfareIncrease) ao)
    else
      if mrsCrossover myMrsBefore traderMrsBefore myMrsAfter traderMrsAfter
        then do -- MRS cross-over, turn offer down
          ao <- agentObservableM
          return (sendEventTo traderId (TradingReply $ Refuse MRSCrossover) ao)
        else do  
          -- all good, transact and accept offer
          transactTradeWealth myId sugEx spiEx

          ao <- agentObservableM
          return (sendEventTo traderId (TradingReply Accept) ao)

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
                    => AgentId
                    -> Double
                    -> Double
                    -> AgentAction g ()
transactTradeWealth myId sugEx spiEx = do
  -- NOTE: negative values shouldn't happen due to welfare increase / MRS crossover restrictions
  -- but for security reasons make sure that we cap at 0 and cant go below
  updateAgentState (\s -> s { sugAgSugarLevel = max (sugAgSugarLevel s + sugEx) 0
                            , sugAgSpiceLevel = max (sugAgSpiceLevel s + spiEx) 0 })

  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteWithOccupier myId
 