module HACClassicParBackend where

import HACAgent as Agent

import Control.DeepSeq
import Control.Monad.Par

processPar :: [AgentState] -> [AgentState]
processPar asIn = runPar p
    where
        p = parMap (processAgent (force asIn)) asIn

-- TODO: need a step-with per time-unit, would require the time expired since last iteration => explicitly modelling
--       time but this would then be real-time!
processAgent :: [AgentState] -> AgentState -> AgentState
processAgent allAgents a = a { agentPos = newPos }
    where
        friendPos = agentPos $ allAgents !! friend a
        enemyPos = agentPos $ allAgents !! enemy a
        newPos = decidePosition friendPos enemyPos a