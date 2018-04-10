{-# LANGUAGE Arrows #-}
module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa
import System.Random

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import ABS
import SD
import SIR

instance Arbitrary SIRState where
  -- arbitrary :: Gen SIRState
  arbitrary = elements [Susceptible, Infected, Recovered]

main :: IO ()
main = do
  g <- getStdGen
  testDynamics g
  --defaultMain (tests g)
  
tests :: RandomGen g
      => g 
      -> TestTree
tests g = testGroup "SIR ABS Tests" [propTests g]

propTests :: RandomGen g
          => g 
          -> TestTree
propTests g = 
  testGroup 
    "SIR ABS property tests"
      [test_agent_behaviour_quickgroup g]

test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ test_agent_behaviour_susceptible g
      , test_agent_behaviour_infected g ]

test_agent_behaviour_susceptible g
  = QC.testProperty "susceptible behaviour" (testCaseSusceptible g)

test_agent_behaviour_infected g
  = QC.testProperty "infected behaviour" (testCaseInfected g)

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
-- NOTE: this is black-box verification
testCaseSusceptible :: RandomGen g
                    => g 
                    -> [SIRState]
                    -> Bool
testCaseSusceptible g0 otherAgents = diff <= eps
  where
    n    = 10000 -- TODO: how to select a 'correct' number of runs?
    eps  = 0.1   -- TODO: how to select a 'correct' epsilon? probably it depends on the ratio between dt and contact rate?
    dt   = 0.1   -- TODO: how to select a 'correct' dt? when the other parameters (n and eps) are selected 'correctly' then adjust dt until the test succeeds
    diff = testSusceptible g0 n dt

    testSusceptible :: RandomGen g
                    => g      -- ^ initial RNG
                    -> Int    -- ^ number of runs
                    -> DTime  -- ^ time-delta to use
                    -> Double -- ^ returns difference to target
    testSusceptible g0 n dt 
        = abs (target - countFract)
      where
        -- we have 3 other agents, each in one of the states
        -- this means, that this susceptible agent will pick
        -- on average an Infected with a probability of 1/3
        otherAgentsCount  = length otherAgents
        infOtherAgents    = length $ filter (Infected==) otherAgents
        infToNonInfRatio  
          -- prevent division by zero
          = if 0 == otherAgentsCount 
              then 0
              else fromIntegral infOtherAgents / fromIntegral otherAgentsCount

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
-- NOTE: this is black-box verification
testCaseInfected :: RandomGen g 
                 => g
                 -> Bool
testCaseInfected g0 = diff <= eps
  where
    n    = 10000 -- TODO: how to select a 'correct' number of runs?
    eps  = 0.5   -- TODO: how to select a 'correct' epsilon? probably it depends on ratio between dt and illnessduration?
    dt   = 0.1   -- TODO: how to select a 'correct' dt? when the other parameters (n and eps) are selected 'correctly' then adjust dt until the test succeeds
    diff = testInfected g0 n dt

    testInfected :: RandomGen g 
                => g      -- ^ initial RNG
                -> Int    -- ^ number of runs
                -> DTime  -- ^ time-delta to use
                -> Double   -- ^ difference to target
    testInfected g0 n dt = abs (target - durationsAvg)
      where
        durations    = testInfectedAux g0 n []
        durationsAvg = sum durations / fromIntegral (length durations)
        
        target = illnessDuration
        
        testInfectedAux :: RandomGen g 
                        => g
                        -> Int
                        -> [Double]
                        -> [Double]
        testInfectedAux _ 0 acc = acc
        testInfectedAux g n acc 
            = testInfectedAux g'' (n-1) acc'
          where
            (g', g'')    = split g
            steps        = repeat (dt, Nothing)
            evts         = embed (testInfectedSF g') ((), steps)
            -- there will always be an event, testInfectedSF will eventuall return an event 
            (Event t)    = fromJust $ find isEvent evts
            acc'         = t : acc

        testInfectedSF :: RandomGen g 
                      => g
                      -> SF () (Event Time)
        testInfectedSF g = proc _ -> do
          ret <- infectedAgent g -< []
          t <- time -< ()
          case ret of 
            Susceptible -> returnA -< NoEvent -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code
            Infected    -> returnA -< NoEvent
            Recovered   -> returnA -< Event t 

-- | Testing behaviour of recovered agent
--   A correct recovered agent will stay recovered
--   forever, which cannot be tested through 
--   computation. We can reason that it is correct
--   simply by looking at the code, which 
--   is so simple (1 line) that it is correct 
--   by definition: its basically a constant function
--   We indicated by Void and undefined that this
--   test does not make sense.
testCaseRecovered :: Void
testCaseRecovered = undefined

-- | Compare the dynamics of ABS and SD approach
--   should be on average the same
--   we are calculating a large number of ABS
--   replications N and average them, this will
--   then be compared to the SD dynamics (note
--   that we only need to calculate the SD 
--   dynamics once, because they are not stochastic)
-- TODO: doesnt really work... comparing SD and ABS dynamics is 
--       very difficult on a quantitative level due to ABS inherent
--       stochastics and different nature
testDynamics :: RandomGen g 
             => g
             -> IO ()
testDynamics g = do
    let popSize = 100 :: Double
    let infCount = 1   :: Double

    let sdDyns  = runSD popSize infCount 150 0.1
    absDynss <- forM ([1..1] :: [Int]) (\_ -> do
      g' <- newStdGen
      return $ runABS g' (floor popSize) (floor infCount) 150 0.1)

    let absDynssFiltered = filter okDynamics absDynss
    let absDyns = averageAbsDynamics absDynssFiltered

    let (sdSus, sdInf, sdRec)    = unzip3 sdDyns
    let (absSus, absInf, absRec) = unzip3 absDyns

    let nmseSus = nmse sdSus absSus
    let nmseInf = nmse sdInf absInf
    let nmseRec = nmse sdRec absRec

    let sdfilename  = "sd_" ++ show popSize ++ ".m"
    let absfilename = "abs_" ++ show popSize ++ ".m"

    writeAggregatesToFile sdfilename sdDyns
    writeAggregatesToFile absfilename absDyns

    print (nmseSus, nmseInf, nmseRec)

  where
    okDynamics :: [(Double, Double, Double)] -> Bool
    okDynamics = (>50) . third . last 
      where
        third (_, _, x) = x

    -- | Normalized Mean Square Error
    -- Assuming xs and ys are same length 
    nmse :: [Double]  -- ^ xs
         -> [Double]  -- ^ ys
         -> Double    -- ^ nmse 
    nmse xs ys = nmseSum / n
      where
        n = fromIntegral $ length xs
        xsNorm = sum xs / n
        ysNorm = sum ys / n

        nmseSum = sum $ zipWith (\x y -> ((x - y) ** 2) / (xsNorm * ysNorm)) xs ys

    -- | Averaging, but ignoring cases where the infected recover
    -- without really starting the dynamics
    averageAbsDynamics :: [[(Double, Double, Double)]]
                       -> [(Double, Double, Double)]
    averageAbsDynamics []         = []
    averageAbsDynamics (ds : dss) = dsAvgs
      where
        -- TODO filter out "invalid dynamics"
        n = 1 + fromIntegral (length dss)
        dsSums = sumAbsDyns dss ds 
        dsAvgs = map (\(ss, is, rs) -> (ss / n, is / n, rs / n)) dsSums

        sumAbsDyns :: [[(Double, Double, Double)]]
                   -> [(Double, Double, Double)]
                   -> [(Double, Double, Double)]
        sumAbsDyns []  acc        = acc
        sumAbsDyns (ds : dss) acc = sumAbsDyns dss acc'
          where
            acc' = zipWith tripleSum ds acc

            tripleSum :: (Double, Double, Double)
                      -> (Double, Double, Double)
                      -> (Double, Double, Double)
            tripleSum (x1, y1, z1) (x2, y2, z2) 
              = (x1 + x2, y1 + y2, z1 + z2)      