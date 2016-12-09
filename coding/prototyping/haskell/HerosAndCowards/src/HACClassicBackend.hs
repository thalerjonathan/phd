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
processIO :: Sim.SimIn -> (Sim.SimOut -> IO (Bool, Double)) -> IO ()
processIO simIn outFunc = do
        simOut <- process' asInit outFunc 0.0
        return ()
    where
        asInit = Sim.simInInitAgents simIn
        wt = Sim.simInWorldType simIn

        process' :: [Agent.AgentState] -> (Sim.SimOut -> IO (Bool, Double)) -> Double -> IO Sim.SimOut
        process' as outFunc dt = do
                let (simOut, as') = nextStep as dt wt
                (continue, dt) <- outFunc simOut
                if continue then
                 process' as' outFunc dt
                     else
                         return simOut

processSteps :: Sim.SimIn -> Double -> Int -> [Sim.SimOut]
processSteps simIn dt steps = processSteps' asInit dt steps
    where
        asInit = Sim.simInInitAgents simIn
        wt = Sim.simInWorldType simIn

        processSteps' :: [Agent.AgentState] -> Double -> Int -> [Sim.SimOut]
        processSteps' as dt steps
            | steps > 0 = [simOut] ++ processSteps' as' dt (steps - 1)
            | otherwise = [simOut]
                where
                    (simOut, as') = nextStep as dt wt

------------------------------------------------------------------------------------------------------------------------
-- MONADIC Agent implementation
------------------------------------------------------------------------------------------------------------------------
{-
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
        -}
------------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
nextStep :: [Agent.AgentState] -> Double -> Agent.WorldType -> (Sim.SimOut, [Agent.AgentState])
nextStep as dt wt = (simOut, as')
    where
        aos = allAgentSteps as dt wt
        as' = map Agent.agentOutState aos
        simOut = Sim.SimOut { Sim.simOutAgents = aos }

allAgentSteps :: [Agent.AgentState] -> Double -> Agent.WorldType -> [Agent.AgentOut]
allAgentSteps = allAgentStepsSeq -- NOTE: can be replaced by allAgentStepsPar / allAgentStepsSeq

allAgentStepsPar :: [Agent.AgentState] -> Double -> Agent.WorldType -> [Agent.AgentOut]
allAgentStepsPar as stepWidth wt = runPar p
    where
        ains = Agent.agentInFromAgents as
        p = parMap (Agent.agentStep wt stepWidth) ains

allAgentStepsSeq :: [Agent.AgentState] -> Double -> Agent.WorldType -> [Agent.AgentOut]
allAgentStepsSeq as stepWidth wt = p
    where
        ains = Agent.agentInFromAgents as
        p = map (Agent.agentStep wt stepWidth) ains
