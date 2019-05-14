{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE FunctionalDependencies #-}
module SIR.API where

type Time       = Double
type AgentId    = Int
newtype Event e = Event e deriving Show

-- encapsulates the effectful API for agents
class Monad m => MonadAgent e m | m -> e where
  randomBool  :: Double -> m Bool
  randomExp   :: Double -> m Double
  randomElem  :: [a] -> m a
  getAgentIds :: m [AgentId]
  getTime     :: m Time
  getMyId     :: m AgentId
  schedEvent  :: AgentId -> e -> Double -> m ()
  sendSync    :: AgentId -> e -> m (Maybe [e])