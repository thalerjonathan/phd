{-# LANGUAGE Arrows #-}
module ABSFeedbackTests 
  (
    absFeedbackTests
  ) where

-- import Control.Monad
import Data.List
import Data.Maybe
import Data.Void
import FRP.Yampa
--import System.Random

import Test.Tasty
import Test.Tasty.QuickCheck as QC

--import Statistics.Distribution          as Stat
--import Statistics.Distribution.StudentT as StudT

import ABSFeedback
-- import SD
import SIR

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

absFeedbackTests :: RandomGen g
                 => g 
                 -> TestTree
absFeedbackTests g 
  = testGroup 
      "SIR ABS Feedback Tests" 
      [ test_agent_behaviour_quickgroup g
      , test_agent_signal_quickgroup g
      ]

test_agent_behaviour_quickgroup :: RandomGen g
                                => g 
                                -> TestTree
test_agent_behaviour_quickgroup g
  = testGroup "agent behaviour"
      [ QC.testProperty "susceptible behaviour" (testCaseSusceptible g)
      , QC.testProperty "infected behaviour" (testCaseInfected g) ]

test_agent_signal_quickgroup :: RandomGen g
                             => g 
                             -> TestTree
test_agent_signal_quickgroup g
  = testGroup "agent signal behaviour"
      [ QC.testProperty "susceptible signal behaviour" (testCaseSusceptibleSignal g)
      , QC.testProperty "infected signal behaviour" (testCaseInfectedSignal g)]

-- | Testing whether a susceptible agent
-- behaves as a signal: does the susceptible agent
-- depen on time, more precicelsy does it NOT change
-- when time does not advance?
testCaseSusceptibleSignal :: RandomGen g
                          => g 
                          -> [SIRState]
                          -> Bool
testCaseSusceptibleSignal g0 otherAgents 
    = 0 == count
  where
    n     = 10000
    count = testSusceptibleSignalAux otherAgents g0 n 0

    testSusceptibleSignalAux :: RandomGen g 
                      => [SIRState]
                      -> g
                      -> Int
                      -> Int
                      -> Int
    testSusceptibleSignalAux _ _ 0 count = count
    testSusceptibleSignalAux otherAgents g n count 
        = testSusceptibleSignalAux otherAgents g'' (n-1) count'
      where
        (g', g'')   = split g

        dt          = 0
        stepsCount  = 100
        steps       = replicate stepsCount (dt, Nothing)

        ret         = embed (testSusceptibleSF otherAgents g') ((), steps)
        gotInfected = True `elem` ret

        count'      = if gotInfected then count + 1 else count

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
        = abs (target - countFract)
      where
        -- we have n other agents, each in one of the states
        -- this means, that this susceptible agent will pick
        -- on average an Infected with a probability of 1/n
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
  ret <- susceptibleAgent g g g g contactRate infectivity illnessDuration -< otherAgents
  case ret of 
    Susceptible -> returnA -< False
    Infected    -> returnA -< True
    Recovered   -> returnA -< False -- TODO: should never occur, can we test this? seems not so, but we can pretty easily guarantee it due to simplicity of code

-- | Testing signal behaviour of infected agent
testCaseInfectedSignal :: RandomGen g 
                       => g
                       -> [SIRState]
                       -> Bool
testCaseInfectedSignal g0 otherAgents 
    = 0 == count
  where
    n     = 10000
    count = testInfectedSignal g0 n 0

    testInfectedSignal :: RandomGen g 
                    => g
                    -> Int
                    -> Int
                    -> Int
    testInfectedSignal _ 0 count = count
    testInfectedSignal g n count 
        = testInfectedSignal g'' (n-1) count'
      where
        (g', g'')    = split g
        dt           = 0
        stepsCount   = 100
        steps        = replicate stepsCount (dt, Nothing)
        evts         = embed (testInfectedSF g' otherAgents) ((), steps)
        mayRec       = find isEvent evts
        count'       = if isJust mayRec then count + 1 else count

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
                 -> [SIRState]
                 -> Bool
testCaseInfected g0 otherAgents = diff <= eps
  where
    n    = 10000 -- TODO: how to select a 'correct' number of runs? conjecture: as n -> inf, so goes the error (eps) to 0
    eps  = 0.5   -- TODO: how to select a 'correct' epsilon? conjecture: probably it depends on ratio between dt and illnessduration?
    dt   = 0.25   -- NOTE: to find out a suitable dt use the test itself: start e.g. with 1.0 and always half when test fail until it succeeds
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
            evts         = embed (testInfectedSF g' otherAgents) ((), steps)
            -- there will always be an event, testInfectedSF will eventuall return an event 
            (Event t)    = fromJust $ find isEvent evts
            acc'         = t : acc

testInfectedSF :: RandomGen g 
              => g
              -> [SIRState]
              -> SF () (Event Time)
testInfectedSF g otherAgents = proc _ -> do
  -- note that the otheragents are fed into the infected agents
  -- but that they are ignored for checking whether the test
  -- has failed or not. from this we can infer, that they
  -- don't play a role in the infected agents behaviour at all
  ret <- infectedAgent g illnessDuration -< otherAgents
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
_testCaseRecovered :: Void
_testCaseRecovered = undefined

{-
-- | Compare the dynamics of ABS and SD approach
--   should be on average the same
--   we are calculating a large number of ABS
--   replications N and average them, this will
--   then be compared to the SD dynamics (note
--   that we only need to calculate the SD 
--   dynamics once, because they are not stochastic)
_testDynamics :: RandomGen g 
             => g
             -> IO ()
_testDynamics _ = do
    let popSize = 100 :: Double
    let infCount = 1   :: Double
    let replications = 100 :: Int

    let sdDyns  = runSD popSize infCount contactRate infectivity illnessDuration 150 0.1

    (sc, ic, rc) <- foldM (\(sc, ic, rc) i -> do
        g' <- newStdGen
        let absDyns = runFeedbackABS g' (floor popSize) (floor infCount) contactRate infectivity illnessDuration 150 0.1
        let (nmseSus, nmseInf, nmseRec) = calcNmse sdDyns absDyns
        let (confSus, confInf, confRec) = calcConf 0.95 sdDyns absDyns
        putStrLn $ "NMSE " ++ show i ++ ": " ++ show (nmseSus, nmseInf, nmseRec)
        putStrLn $ "Conf " ++ show i ++ ": " ++ show (confSus, confInf, confRec)

        let sc' = if nmseSus < 0.1 then sc + 1 else sc
        let ic' = if nmseInf < 0.1 then ic + 1 else ic
        let rc' = if nmseRec < 0.1 then rc + 1 else rc

        return (sc', ic', rc')
      ) ((0,0,0) :: (Int, Int, Int)) [1..replications]

    --let sdfilename  = "sd_" ++ show popSize ++ ".m"
    --let absfilename = "abs_" ++ show popSize ++ ".m"

    --writeAggregatesToFile sdfilename sdDyns
    --writeAggregatesToFile absfilename absDyns

    print (sc, ic, rc)
    
  where
    -- TODO: do confidence intervals make sense here?
    -- also how do we construct them?
    calcConf :: Double
             -> [(Double, Double, Double)]
             -> [(Double, Double, Double)]
             -> (Double, Double, Double)
    calcConf alpha sdDyns absDyns = (tDistSample, tDistSample, tDistSample)
      where
        (sdSus, sdInf, sdRec)    = unzip3 sdDyns
        (absSus, absInf, absRec) = unzip3 absDyns

        -- assuming all are same length
        n = fromIntegral $ length sdSus

        sdSusMean = ABSFeedbackTests.mean sdSus
        sdSusStd = ABSFeedbackTests.std sdSus

        absSusMean = ABSFeedbackTests.mean absSus
        absSusStd = ABSFeedbackTests.std absSus

        degFree = n
        tDist = StudT.studentT degFree
        -- TODO: does not return the required value
        tDistSample = Stat.density tDist ((1 - alpha) / 2)

    calcNmse :: [(Double, Double, Double)]
             -> [(Double, Double, Double)]
             -> (Double, Double, Double)
    calcNmse sdDyns absDyns = (nmseSus, nmseInf, nmseRec)
      where
        (sdSus, sdInf, sdRec)    = unzip3 sdDyns
        (absSus, absInf, absRec) = unzip3 absDyns

        nmseSus = nmse sdSus absSus
        nmseInf = nmse sdInf absInf
        nmseRec = nmse sdRec absRec

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

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

std :: [Double] -> Double
std xs = sqrt $ sum (map (\x -> (x - x') ** 2) xs) / n
  where
    x' = ABSFeedbackTests.mean xs 
    n = fromIntegral (length xs) - 1

-- | Averaging, but ignoring cases where the infected recover
-- without really starting the dynamics
averageAbsDynamics :: [[(Double, Double, Double)]]
                    -> [(Double, Double, Double)]
averageAbsDynamics []         = []
averageAbsDynamics (ds : dss) = dsAvgs
  where
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
-}