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
                (simOut, as') <- nextStep as dt wt
                (continue, dt) <- outFunc simOut
                if continue then
                 process' as' outFunc dt
                     else
                         return simOut

processSteps :: Sim.SimIn -> Double -> Int -> IO [Sim.SimOut]
processSteps simIn dt steps = processSteps' asInit dt steps
    where
        asInit = Sim.simInInitAgents simIn
        wt = Sim.simInWorldType simIn

        processSteps' :: [Agent.AgentState] -> Double -> Int -> IO [Sim.SimOut]
        processSteps' as dt steps = do
            (simOut, as') <- nextStep as dt wt
            if (steps > 0) then do
                    recs <- processSteps' as' dt (steps - 1)
                    return ([simOut] ++ recs)
                else
                    return [simOut]

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
nextStep :: [Agent.AgentState] -> Double -> Agent.WorldType -> IO (Sim.SimOut, [Agent.AgentState])
nextStep as dt wt = do
    aos <- allAgentStepsSeq as dt wt
    let as' = map Agent.agentOutState aos
    let simOut = Sim.SimOut { Sim.simOutAgents = aos }
    return (simOut, as')

-- NOTE: can not run in parallel due to referential updates through IORef
allAgentStepsSeq :: [Agent.AgentState] -> Double -> Agent.WorldType -> IO [Agent.AgentOut]
allAgentStepsSeq as stepWidth wt = p
    where
        ains = Agent.agentInFromAgents as
        p = mapM (Agent.agentStep wt stepWidth) ains
