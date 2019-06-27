{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SIR.API where

type Time    = Double
type AgentId = Int

-- encapsulates the effectful API for agents
class Monad m => MonadAgent e m | m -> e where
  randomBool  :: Double -> m Bool
  randomExp   :: Double -> m Double
  randomElem  :: [a] -> m a
  getAgentIds :: m [AgentId]
  getTime     :: m Time
  getMyId     :: m AgentId
  schedEvent  :: e -> AgentId -> Double -> m ()

class Monad m => MonadAgentSync e m | m -> e where
  sendSync :: e -> AgentId -> m (Maybe [e])