{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Trading 
  ( agentTrade
  , handleTradingOffer
  ) where

import Data.Maybe

import Control.Monad.Random
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
    ao <- agentOutObservableM
    return (ao, Nothing)
  | otherwise = tradingRound myId globalHdl

tradingRound :: RandomGen g
             => AgentId
             -> EventHandler g
             -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
tradingRound myId globalHdl = do
  myCoord <- agentProperty sugAgCoord
  -- (re-)fetch neighbours from environment, to get up-to-date information
  ns    <- lift $ lift $ neighboursM myCoord False
  myMrs <- mrsM

  -- filter out unoccupied sites and traders with same MRS (VERY unlikely with floating point)
  let potentialTraders = mapMaybe (sugEnvSiteOccupier . snd) $ filter (\(_, ss) -> 
                          case sugEnvSiteOccupier ss of 
                            Nothing  -> False
                            Just occ -> sugEnvOccMRS occ /= myMrs) ns

  if null potentialTraders
    then do
      ao <- agentOutObservableM
      return (ao, Just globalHdl)
    else do
      potentialTraders' <- lift $ lift $ lift $ fisherYatesShuffleM potentialTraders
      tradeWith myId globalHdl False potentialTraders'

tradeWith :: RandomGen g
          => AgentId
          -> EventHandler g
          -> Bool
          -> [SugEnvSiteOccupier]
          -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
tradeWith myId globalHdl tradeOccured []       -- iterated through all potential traders => trading round has finished
  | tradeOccured = tradingRound myId globalHdl -- if we have traded with at least one agent, try another round
  | otherwise    = do
    ao <- agentOutObservableM
    return (ao, Just globalHdl) -- no trading has occured, quit trading and switch back to globalHandler

tradeWith myId globalHdl tradeOccured (trader : ts) = do -- trade with next one
  myMrs  <- mrsM
  currWf <- agentWelfareM

  let otherMrs       = sugEnvOccMRS trader
      (sugEx, spiEx) = computeExchange myMrs otherMrs

  tradeWf <- agentWelfareChangeM sugEx spiEx

  -- TODO: check crossover

  -- NOTE: check if it makes this agent better off: does it increase the agents welfare?
  if tradeWf < currWf
    then tradeWith myId globalHdl tradeOccured ts -- not better off, continue with next trader
    else do
      let evtHandler = tradingHandler myId globalHdl otherMrs tradeOccured ts
          traderId   = sugEnvOccId trader
      ao <- agentOutObservableM
      return (sendEventTo traderId (TradingOffer myMrs) ao, Just evtHandler)

tradingHandler :: RandomGen g
               => AgentId
               -> EventHandler g
               -> Double
               -> Bool
               -> [SugEnvSiteOccupier]
               -> EventHandler g
tradingHandler myId globalHdl0 otherMrs tradeOccured traders = 
    continueWithAfter
      (proc evt -> 
        case evt of
          (DomainEvent (_sender, TradingReply accept)) -> 
            arrM (handleTradingReply globalHdl0) -< accept
          _ -> returnA -< error $ "Agent " ++ show myId ++ ": received unexpected event " ++ show evt ++ " during active Trading, terminating simulation!")
  where
    handleTradingReply :: RandomGen g
                       => EventHandler g
                       -> Bool
                       -> AgentAction g (SugAgentOut g, Maybe (EventHandler g))
    handleTradingReply globalHdl False =  -- the sender refuses the trading-offer, continue with the next trader
      tradeWith myId globalHdl tradeOccured traders
    handleTradingReply globalHdl True = do -- the sender accepts the trading-offer
      myMrs <- mrsM
      let (sugEx, spiEx) = computeExchange myMrs otherMrs
      -- NOTE: at this point the other agent is better off as well and has already transacted
      updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sugEx
                                , sugAgSpiceLevel = sugAgSpiceLevel s + spiEx })
      tradeWith myId globalHdl True traders

handleTradingOffer :: RandomGen g
                   => AgentId
                   -> AgentId
                   -> Double
                   -> AgentAction g (SugAgentOut g)
handleTradingOffer _myId sender otherMrs = do
  myMrs  <- mrsM
  currWf <- agentWelfareM

  let (sugEx, spiEx) = computeExchange myMrs otherMrs

  tradeWf <- agentWelfareChangeM sugEx spiEx

  -- TODO: check crossover

  let acceptTrade = tradeWf > currWf

  when 
    acceptTrade
    (updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sugEx
                               , sugAgSpiceLevel = sugAgSpiceLevel s + spiEx }))

  ao <- agentOutObservableM
  return (sendEventTo sender (TradingReply acceptTrade) ao)

computeExchange :: Double
                -> Double
                -> (Double, Double)
computeExchange myMrs otherMrs 
    | myMrs > otherMrs = (sugEx, -spiEx) -- spice flows from agent with higher mrs (myMrs) to agent with lower (otherMrs) => subtract spice from this agent and add sugar 
    | otherwise        = (-sugEx, spiEx) -- sugar flows from agent with lower mrs (myMrs) to agent with higher (otherMrs) => subtract sugar for this agent and add spice
  where
    (sugEx, spiEx) = exchangeRates myMrs otherMrs
      
exchangeRates :: Double
              -> Double
              -> (Double, Double)
exchangeRates myMrs otherMrs 
    | price > 1 = (1, price)     -- trading p units of spice for 1 unit of sugar
    | otherwise = (1 / price, 1) -- trading 1/p units of sugar for 1 unit of spice
  where
    price = sqrt (myMrs * otherMrs)