{-# LANGUAGE Arrows #-}
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
  myMrs <- mrsM
  let traderMrs = sugEnvOccMRS trader
      traderId  = sugEnvOccId trader

  let _price = sqrt (myMrs * traderMrs)

  -- TODO: check if it makes this agent better off
  -- TODO: how to check if MRS cross over??

  let evtHandler = tradingHandler myId globalHdl tradeOccured ts
  ao <- agentOutObservableM

  return (sendEventTo traderId (TradingOffer myMrs) ao, Just evtHandler)

tradingHandler :: RandomGen g
               => AgentId
               -> EventHandler g
               -> Bool
               -> [SugEnvSiteOccupier]
               -> EventHandler g
tradingHandler myId globalHdl0 tradeOccured traders = 
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
    handleTradingReply globalHdl False =  -- the sender refuse the trading-offer, continue with the next trader
      tradeWith myId globalHdl tradeOccured traders
    handleTradingReply globalHdl True = do -- the sender accepts the trading-offer
      -- TODO: transact trade
      tradeWith myId globalHdl True traders

handleTradingOffer :: RandomGen g
                   => AgentId
                   -> AgentId
                   -> Double
                   -> AgentAction g (SugAgentOut g)
handleTradingOffer _myId _traderId _traderMrs = do
  -- TODO: check if it makes this agent better off
  -- TODO: how to check if MRS cross over??

  agentOutObservableM