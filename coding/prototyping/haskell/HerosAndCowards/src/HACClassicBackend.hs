module HACClassicBackend (
    processIO,
    processSteps
  )where

import System.Random

import qualified HACAgent as Agent
import qualified HACSimulation as Sim

import Control.DeepSeq
import Control.Monad.Par

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
processIO :: [Agent.AgentState] -> (Sim.SimOut -> IO (Bool, Double)) -> IO ()
processIO as outFunc = do
        simOut <- process' as outFunc 0.0
        return ()
    where
        process' :: [Agent.AgentState] -> (Sim.SimOut -> IO (Bool, Double)) -> Double -> IO Sim.SimOut
        process' as outFunc dt = do
                             let stepWidth = Agent.agentSpeedPerTimeUnit * dt
                             let aos = allAgentSteps as stepWidth
                             let as' = map Agent.agentOutState aos
                             let simOut = Sim.SimOut { Sim.simOutAllAgents = aos }
                             (continue, dt) <- outFunc simOut
                             if continue then
                                 process' as' outFunc dt
                                     else
                                         return simOut

processSteps :: [Agent.AgentState] -> Double -> Int -> [Sim.SimOut]
processSteps as dt steps
    | steps > 0 = [simOut] ++ processSteps as' dt (steps - 1)
    | otherwise = [simOut]
        where
            stepWidth = Agent.agentSpeedPerTimeUnit * dt
            aos = allAgentSteps as stepWidth
            as' = map Agent.agentOutState aos
            simOut = Sim.SimOut { Sim.simOutAllAgents = aos }

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

createRandAgents :: RandomGen g => g -> Int -> Double -> Agent [Agent.AgentState]
createRandAgents g n p = mapM return randAgentStates
    where
        randAgentStates = Agent.createRandAgentStates g n p
------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
allAgentSteps :: [Agent.AgentState] -> Double -> [Agent.AgentOut]
allAgentSteps = allAgentStepsSeq -- NOTE: can be replaced by allAgentStepsPar / allAgentStepsSeq

allAgentStepsPar :: [Agent.AgentState] -> Double -> [Agent.AgentOut]
allAgentStepsPar as stepWidth = runPar p
    where
        ains = Agent.agentInFromAgents as
        p = parMap (Agent.agentStep stepWidth) ains

allAgentStepsSeq :: [Agent.AgentState] -> Double -> [Agent.AgentOut]
allAgentStepsSeq as stepWidth = p
    where
        ains = Agent.agentInFromAgents as
        p = map (Agent.agentStep stepWidth) ains
