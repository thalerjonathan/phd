{-# LANGUAGE Arrows #-}
module Main where

import Data.Void
import FRP.Yampa
import System.Random

-- import Debug.Trace

import ABS
--import SD
import SIR

main :: IO ()
main = do
  --let sdDyns  = runSD 150 0.01
  --    absDyns = runABS 150 0.1
  
  --let filename = "sd_" ++ show populationSize ++ ".m"
  --writeAggregatesToFile filename sdDyns

  --print sdDyns
  --print absDyns
  g <- getStdGen

  let n   = 1000
      eps = 0.1
      dt  = 0.1
      ret = testSusceptible g n eps dt

  print ret
  
  return ()


-- | Testing behaviour of susceptible agent
--    a susceptible agent makes on average contact
--    with contact rate other agents per time-unit
--    => when running N susceptible agents for 1.0
--    time-unit then
--    N * infectivity * contactRate * (infectedAgentsCount / totalAgentsCount) 
--    agents should become infected
--   NOTE: this also allows to estimate the dt: to 
--   achieve a sufficiently close match one selects
--   a very small epsilon and then reduces the dt
--   until the average falls into the epsilon environment
testSusceptible :: RandomGen g 
                => g      -- ^ initial RNG
                -> Int    -- ^ number of runs
                -> Double -- ^ epsilon 
                -> DTime  -- ^ time-delta to use
                -> (Bool, Double)   -- ^ True in case test passes
testSusceptible g0 n eps dt = (diff <= eps, diff)
  where
    -- we have 3 other agents, each in one of the states
    -- this means, that this susceptible agent will pick
    -- on average an Infected with a probability of 1/3
    otherAgents       = [Susceptible, Infected, Recovered]
    infOtherAgents    = length $ filter (Infected==) otherAgents
    nonInfOtherAgents = length $ filter (Infected/=) otherAgents
    infToNonInfRatio  = fromIntegral infOtherAgents / fromIntegral nonInfOtherAgents

    count      = testSusceptibleAux otherAgents g0 n 0
    countFract = fromIntegral count / fromIntegral n
    -- multiply with contactRate because we make on 
    -- average contact with contactRate other agents 
    -- per time-unit (we are running the agent for
    -- 1.0 time-unit)
    -- also multiply with ratio of infected to non-infected
    --   TODO: can we extract the formula of the SD approach
    --   here? should be possible, then we can prove that our
    --   ABS approach is indeed a valid SD approximation
    --   (due to averaging) 
    target = infectivity * contactRate * infToNonInfRatio
    diff   = abs (target - countFract)

    testSusceptibleAux :: RandomGen g 
                       => [SIRState]
                       -> g
                       -> Int
                       -> Int
                       -> Int
    testSusceptibleAux _ _ 0 count = count
    testSusceptibleAux otherAgents g n count 
        = testSusceptibleAux otherAgents g'' (n-1) count'
      where
        (g', g'')   = split g

        stepsCount  = floor (1.0 / dt)
        steps       = replicate stepsCount (dt, Nothing)

        ret         = embed (testSusceptibleSF otherAgents g') ((), steps)
        gotInfected = True `elem` ret

        count'      = if gotInfected then count + 1 else count

    testSusceptibleSF :: RandomGen g 
                      => [SIRState]
                      -> g
                      -> SF () Bool
    testSusceptibleSF otherAgents g = proc _ -> do
      ret <- susceptibleAgent g -< otherAgents
      case ret of 
        Susceptible -> returnA -< False
        Infected    -> returnA -< True
        Recovered   -> returnA -< False -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code

-- | Testing behaviour of infected agent
--   run test until agent recovers, which happens
--   on average after illnessDuration
--   we are running this test a larger number of 
--   times N and averaging the durations of all
--   agents until their recovery
--   should be within an epsilon of illnessDuration
testInfected :: ()
testInfected = undefined

-- | Testing behaviour of recovered agent
--   A correct recovered agent will stay recovered
--   forever, which cannot be tested through 
--   computation. We can reason that it is correct
--   simply by looking at the code, which 
--   is so simple (1 line) that it is correct 
--   by definition: its basically a constant function
--   We indicated by Void and undefined that this
--   test does not make sense.
testRecovered :: Void
testRecovered = undefined

-- | Compare the dynamics of ABS and SD approach
--   should be on average the same
--   we are calculating a large number of ABS
--   replications N and average them, this will
--   then be compared to the SD dynamics (note
--   that we only need to calculate the SD 
--   dynamics once, because they are not stochastic)
testDynamics :: ()
testDynamics = undefined