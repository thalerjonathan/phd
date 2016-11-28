module HACClassicBackend (
    process,
    process_
  )where

import HACAgent as Agent

import Control.DeepSeq
import Control.Monad.Par

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
process :: [AgentState] -> ([AgentState] -> IO Bool) -> IO [AgentState]
process as outFunc = do
    let as' = processAgentsPar as
    continue <- outFunc as'
    if continue then
        process as' outFunc
            else
                return as'

process_ :: [AgentState] -> Int -> [AgentState]
process_ as iterCount
    | iterCount > 0 = process_ as' (iterCount - 1)
    | otherwise = as'
        where
            as' = processAgentsPar as

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
processAgentsPar :: [AgentState] -> [AgentState]
processAgentsPar as = runPar p
    where
        p = parMap (processAgent as) as

processAgentsSeq :: [AgentState] -> [AgentState]
processAgentsSeq as = p
    where
        p = map (processAgent as) as

-- TODO: need a step-with per time-unit, would require the time expired since last iteration => explicitly modelling
--       time but this would then be real-time!
processAgent :: [AgentState] -> AgentState -> AgentState
processAgent allAgents a = a { agentPos = newPos }
    where
        friendPos = agentPos $ allAgents !! friend a
        enemyPos = agentPos $ allAgents !! enemy a
        newPos = decidePosition friendPos enemyPos a