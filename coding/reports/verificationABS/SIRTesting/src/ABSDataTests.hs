{-# LANGUAGE Arrows #-}
module ABSDataTests 
  (
    absDataTests
  ) where

import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import ABSData
import SIR

import Debug.Trace

-- note that we are creating also recovered messages
-- although a recovered agent never actually generates
-- one, but we need them to cover all cases
instance Arbitrary SIRMsg where
  -- arbitrary :: Gen SIRMsg
  arbitrary = elements [Contact Susceptible, Contact Infected, Contact Recovered]

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

absDataTests :: RandomGen g
             => g 
             -> TestTree
absDataTests g 
  = testGroup 
      "SIR ABS Data Tests" 
      [ test_agent_behaviour_quickgroup g
      --, test_agent_signal_quickgroup g
      ]

test_agent_behaviour_quickgroup :: RandomGen g
                                => g 
                                -> TestTree
test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ QC.testProperty "susceptible behaviour" (testCaseSusceptible g)
      , QC.testProperty "interaction behaviour" (testCaseInteraction g) ]

-- | Testing the interaction between agents
-- test the interconnection of susceptible and infected agents
-- because the dynamics depend on the correct interplay between them
-- this is but a normal unit-test
-- TODO: implement
testCaseInteraction :: RandomGen g
                    => g 
                    -> Bool
testCaseInteraction _g0 = True 

-- | Testing the susceptible agent
-- in this implementation, the number of infections depends solely 
-- on the number of Contact Infected messages (randomize uisng quickcheck)
-- also the agent should generate on average contactRate Contact Susceptible messages
-- Note that we are not testing whether the susceptible agent generates 
-- contact messages here as this is done in the interaction test
testCaseSusceptible :: RandomGen g
                    => g 
                    -> [AgentId]
                    -> [SIRDataFlow]
                    -> Bool
testCaseSusceptible g0 ais df = diff <= eps
  where
    n    = 10000 -- TODO: how to select a 'correct' number of runs? conjecture: n -> infinity lets go the error (eps) to 0
    eps  = 0.1   -- TODO: how to select a 'correct' epsilon? conjecture: probably it depends on the ratio between dt and contact rate?
    dt   = 0.125   -- NOTE: to find out a suitable dt use the test itself: start e.g. with 1.0 and always half when test fail until it succeeds
    diff = testSusceptible g0 n dt

    testSusceptible :: RandomGen g
                    => g      -- ^ initial RNG
                    -> Int    -- ^ number of runs
                    -> DTime  -- ^ time-delta to use
                    -> Double -- ^ returns difference to target
    testSusceptible g0 n dt 
        = trace 
            ("df = " ++ show df ++ ", countFract = " ++ show countFract ++ " target = " ++ show target)
            (abs (target - countFract))
      where
        totalContacts   = length df
        infContacts     = length $ filter isInfectedContact df
        
        count      = testSusceptibleAux g0 n 0
        countFract = fromIntegral count / fromIntegral n
        
        -- IMPORTANT: in this test-case the number of infected does NOT
        -- depend on the contact rate!!!! this will only be tested in
        -- the combined test of susceptible and infected
        -- Note that in the end in this data-driven implementation,
        -- the probability of a susceptible agent becoming infected
        -- follows a bernoulli trial: for each Contact Infected
        -- message, the agent gets infected with a probability of 
        -- infectivity, thus we have success or fail with complementary
        -- probability which add up to 1 => use bernoulli formula to
        -- calculate the probability of being infected after N Contact
        -- Infected messages => this is a binomial experiment (https://en.wikipedia.org/wiki/Bernoulli_trial):
        -- "which consists of a fixed number {\displaystyle n} n of statistically independent Bernoulli trials, 
        -- each with a probability of success {\displaystyle p} p, and counts the number of successes."
        -- The probability of exactly k successes in an experiment is given by
        -- P(k) = (n over k) p^k q^(n-k), where n is total number of trials, p is prob of success and q = 1-p
        -- in the susceptible case we are interested in the probability of at least one, which means
        -- we have to add up P(1) to P(number of Contact Infected Messages)
        -- TODO: this is for a single step, independent of dt, the more time-steps, the more likely it
        -- is that an agent gets infected => we have a binomial experiment again
        target = sum $ map (binomDistr totalContacts infectivity) [1..infContacts]

        binomDistr :: Int
                   -> Double 
                   -> Int
                   -> Double
        binomDistr n p k = fromIntegral (binom n k) * (p ** fromIntegral k) * (1 - p) ** fromIntegral (n - k)
          where
            binom :: Int -> Int -> Int
            binom _ 0 = 1
            binom 0 _ = 0
            binom n k = binom (n-1) (k-1) * n `div` k

        isInfectedContact :: SIRDataFlow -> Bool
        isInfectedContact (_, Contact Infected) = True
        isInfectedContact _ = False

        testSusceptibleAux :: RandomGen g 
                          => g 
                          -> Int
                          -> Int
                          -> Int
        testSusceptibleAux _ 0 count = count
        testSusceptibleAux g n count 
            = testSusceptibleAux g'' (n-1) count'
          where
            (g', g'')   = split g

            steps       = replicate 1 (dt, Nothing)

            ret         = embed (testSusceptibleSF g' n ais df) ((), steps)
            gotInfected = True `elem` ret

            count'      = if gotInfected then count + 1 else count

testSusceptibleSF :: RandomGen g 
                  => g
                  -> AgentId
                  -> [AgentId]
                  -> [SIRDataFlow]
                  -> SF () Bool
testSusceptibleSF g aid ais df = proc _ -> do
  let ain = (agentIn aid) { aiData = df } 

  ret <- susceptibleAgent g contactRate infectivity illnessDuration ais -< ain
  case aoObservable ret of 
    Susceptible -> returnA -< False
    Infected    -> returnA -< True
    Recovered   -> returnA -< False -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code