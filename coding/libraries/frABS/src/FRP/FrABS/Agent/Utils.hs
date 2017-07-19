module FRP.FrABS.Agent.Utils (
    agentRandomNeighbourNode
  ) where

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Agent.Monad
import FRP.FrABS.Agent.Random
import FRP.FrABS.Environment.Network

import Control.Monad.Trans.State

agentRandomNeighbourNode :: Network l -> State (AgentOut s m (Network l)) AgentId
agentRandomNeighbourNode e = agentIdM >>= \aid -> runAgentRandomM (pickRandomNeighbourNode aid e)