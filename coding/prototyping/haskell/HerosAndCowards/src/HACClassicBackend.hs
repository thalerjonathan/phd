module HACClassicBackend (
    process,
    process_
  )where

import System.Random

import HACAgent as Agent

import Control.DeepSeq
import Control.Monad.Par

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
process :: [AgentState] -> ([AgentOut] -> IO (Bool, Double)) -> IO [AgentOut]
process as outFunc = process' as outFunc 0.0
    where
        process' :: [AgentState] -> ([AgentOut] -> IO (Bool, Double)) -> Double -> IO [AgentOut]
        process' as outFunc dt = do
                                     let distance = Agent.agentSpeedPerTimeUnit * dt
                                     let aos = allAgentSteps as distance
                                     let as' = map agentOutState aos
                                     (continue, dt) <- outFunc aos
                                     if continue then
                                         process' as' outFunc dt
                                             else
                                                 return aos

process_ :: [AgentState] -> Int -> [AgentOut]
process_ as iterCount
    | iterCount > 0 = process_ as' (iterCount - 1)
    | otherwise = aos
        where
            stepWidth = Agent.agentSpeedPerTimeUnit
            aos = allAgentSteps as stepWidth
            as' = map agentOutState aos

------------------------------------------------------------------------------------------------------------------------
-- MONADIC Agent implementation
------------------------------------------------------------------------------------------------------------------------
data Agent a = Agent a

instance Functor Agent where
    fmap f (Agent a) = Agent (f a)

instance Applicative Agent where
    pure a = Agent a
    (Agent f) <*> a = fmap f a

instance Monad Agent where
    return = pure
    Agent a >>= f = f a

createRandAgents :: RandomGen g => g -> Int -> Double -> Agent [AgentState]
createRandAgents g n p = mapM return randAgentStates
    where
        randAgentStates = Agent.createRandAgentStates g n p
------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
allAgentSteps :: [AgentState] -> Double -> [AgentOut]
allAgentSteps = allAgentStepsSeq -- NOTE: can be replaced by allAgentStepsPar / allAgentStepsSeq

allAgentStepsPar :: [AgentState] -> Double -> [AgentOut]
allAgentStepsPar as stepWidth = runPar p
    where
        ains = agentInFromAgents as
        p = parMap (Agent.agentStep stepWidth) ains

allAgentStepsSeq :: [AgentState] -> Double -> [AgentOut]
allAgentStepsSeq as stepWidth = p
    where
        ains = agentInFromAgents as
        p = map (Agent.agentStep stepWidth) ains
