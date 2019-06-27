{-# LANGUAGE Arrows                     #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
module SIR.Agent
  ( sirAgent
  ) where

-- import Control.Monad
import Control.Monad.Trans.MSF.Except
import Data.MonadicStreamFunction

import SIR.API
import SIR.Model

makeContactInterval :: Double
makeContactInterval = 1.0

-- | A sir agent which is in one of three states
sirAgent :: (MonadAgent SIREvent m, MonadAgentSync SIREvent m)
         => Int         -- ^ the contact rate
         -> Double      -- ^ the infectivity
         -> Double      -- ^ the illness duration
         -> SIRState    -- ^ the initial state of the agent
         -> m (MSF m SIREvent SIRState)
sirAgent cor inf ild Susceptible = do
  -- on start
  scheduleMakeContactM 
  return $ susceptibleAgent cor inf ild 
sirAgent _ _ ild Infected = do
  -- on start
  scheduleRecoveryM ild
  return infectedAgent 
sirAgent _ _ _ Recovered = 
  return recoveredAgent

--------------------------------------------------------------------------------
-- AGENTS
--------------------------------------------------------------------------------
susceptibleAgent :: (MonadAgent SIREvent m, MonadAgentSync SIREvent m)
                 => Int
                 -> Double
                 -> Double
                 -> MSF m SIREvent SIRState
susceptibleAgent cor inf ild = 
    switch
      susceptibleAgentInfected
      (const infectedAgent)
  where
    susceptibleAgentInfected :: (MonadAgent SIREvent m, MonadAgentSync SIREvent m) 
                             => MSF m SIREvent (SIRState, Maybe ()) 
    susceptibleAgentInfected = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Susceptible, ret)
        _       -> returnA -< (Infected, ret)

    -- handleEvent :: MonadAgent SIREvent m
    --             => SIREvent 
    --             -> m (Maybe ())
    -- handleEvent (Contact _ Infected) = do
    --   r <- randomBool inf
    --   if r 
    --     then do
    --       scheduleRecoveryM ild
    --       return $ Just ()
    --     else return Nothing

    -- handleEvent MakeContact = do
    --   ais       <- getAgentIds
    --   receivers <- forM [1..cor] (const $ randomElem ais)
    --   mapM_ makeContactWith receivers
    --   scheduleMakeContactM
    --   return Nothing

    -- makeContactWith :: MonadAgent SIREvent m => AgentId -> m ()
    -- makeContactWith receiver = do
    --   ai <- getMyId
    --   schedEvent (Contact ai Susceptible) receiver 0

    handleEvent MakeContact = do
      ais        <- getAgentIds
      ai         <- getMyId
      isInfected <- makeContact cor ai ais
      if isInfected
        then return $ Just ()
        else do
          scheduleMakeContactM
          return Nothing

    handleEvent _ = return Nothing

    makeContact :: (MonadAgent SIREvent m, MonadAgentSync SIREvent m) 
                => Int -> AgentId -> [AgentId] -> m Bool
    makeContact 0 _ _ = return False
    makeContact n ai ais = do
      receiver <- randomElem ais
      if ai == receiver
        then makeContact (n-1) ai ais
        else do
          ret <- makeContactWith receiver
          if ret
            then return True
            else makeContact (n-1) ai ais

    makeContactWith :: (MonadAgent SIREvent m, MonadAgentSync SIREvent m) 
                    => AgentId -> m Bool
    makeContactWith receiver = do
      ai     <- getMyId
      retMay <- sendSync (Contact ai Susceptible) receiver

      case retMay of 
        Nothing -> return False
        (Just es) -> do
          let fromInf = any (\(Contact _ s) -> s == Infected) es
          if not fromInf
            then return False
            else do
              r <- randomBool inf
              if r 
                then do
                  scheduleRecoveryM ild
                  return True
                else return False

infectedAgent :: MonadAgent SIREvent m => MSF m SIREvent SIRState
infectedAgent = 
    switch
      infectedAgentRecovered 
      (const recoveredAgent)
  where
    infectedAgentRecovered :: MonadAgent SIREvent m
                           => MSF m SIREvent (SIRState, Maybe ()) 
    infectedAgentRecovered = proc e -> do
      ret <- arrM handleEvent -< e
      case ret of
        Nothing -> returnA -< (Infected, ret)
        _       -> returnA -< (Recovered, ret)

    handleEvent :: MonadAgent SIREvent m => SIREvent -> m (Maybe ())
    handleEvent (Contact sender Susceptible) = do
      replyContact sender
      return Nothing
    handleEvent Recover = return $ Just ()
    handleEvent _ = return Nothing

    replyContact :: MonadAgent SIREvent m => AgentId -> m ()
    replyContact receiver = do
      ai <- getMyId
      schedEvent (Contact ai Infected) receiver 0

recoveredAgent :: MonadAgent SIREvent m => MSF m SIREvent SIRState
recoveredAgent = arr (const Recovered)

--------------------------------------------------------------------------------
-- AGENT UTILS
--------------------------------------------------------------------------------
scheduleMakeContactM :: MonadAgent SIREvent m => m ()
scheduleMakeContactM = do
  ai <- getMyId
  schedEvent MakeContact ai makeContactInterval

scheduleRecoveryM :: MonadAgent SIREvent m => Double -> m ()
scheduleRecoveryM ild = do
  dt <- randomExp (1 / ild)
  ai <- getMyId
  schedEvent Recover ai dt