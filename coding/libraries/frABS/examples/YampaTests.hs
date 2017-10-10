module YampaTests 
    (
      testYampa
    , testOccasionally
    , testAfterExp
    ) where

import Data.Maybe
import Data.List
import Text.Printf
import System.Random

import FRP.Yampa
import FRP.Yampa.InternalCore
import FRP.FrABS

testYampa :: IO ()
testYampa = testOccasionally

testOccasionally :: IO ()
testOccasionally = do
    let eventFreq = 5   :: DTime
    let dt = 1 / 1         :: DTime
    let t = 1000            :: DTime

    let eventsPerTimeUnit = 1 / eventFreq
    let ecsTheory = t * eventsPerTimeUnit

    let reps = 1
    let steps = floor (t / dt) :: Int

    ecs <- mapM (runOccasionally eventFreq dt steps) [1..reps]

    let ecAvg = fromIntegral (sum ecs) / fromIntegral reps :: Double

    let ecsRatios = map (\ec -> fromIntegral ec / fromIntegral steps) ecs
    let avgRatio = sum ecsRatios / fromIntegral reps :: Double
    let avgRatioTheory = dt / eventFreq
    
    print $ "running with dt of " 
            ++ printf "%.3f" dt ++ " for " 
            ++ show steps ++ " steps with "
            ++ show eventsPerTimeUnit ++ " events per time-unit"
            ++ " for a total time of " ++ show t
    print $ "average event-count = " ++ printf "%.2f" ecAvg ++ ", theoretical average = " ++ printf "%.2f" ecsTheory
    -- print $ "average event-ratio = " ++ printf "%.3f" avgRatio ++ ", theoretical average = " ++ printf "%.3f" avgRatioTheory
    
testAfterExp :: IO ()
testAfterExp = do
    let eventTime = 1 / 5      :: DTime
    let dt = 1 / 1         :: DTime
    let reps = 10000

    ts <- mapM (runAfterExp eventTime dt) [1..reps]

    let avgTs = sum ts / fromIntegral (length ts)

    print $ "event occured on average after = " 
            ++ printf "%.3f" avgTs  
            ++ ", theoretical time: " ++ show eventTime

runOccasionally :: DTime -> DTime -> Int ->  Int -> IO Int
runOccasionally eventFreq dt steps _seed = do
    --let g = mkStdGen 0
    g <- newStdGen
    let sf = occasionally g eventFreq () -- RandomGen g => g -> Time -> b -> SF a (Event b)

    let deltas = replicate steps (dt, Nothing)
    let bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let eventCount = (length . filter isEvent) bs
    let totalCount = length bs
    let ratio = (fromIntegral eventCount / fromIntegral totalCount) :: Double
    --print (show steps ++ " steps, eventFreq = " ++ show eventFreq 
    --        ++ " dt = " ++ show dt 
    --        ++ " ratio = " ++ printf "%.3f" ratio)

    return eventCount

runAfterExp :: DTime -> DTime -> Int -> IO DTime
runAfterExp expEventTime dt _seed = do
    g <- newStdGen
    --let g = mkStdGen _seed
    let sf = afterExp g expEventTime ()

    let deltas = repeat (dt, Nothing)
    let bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let firstEventIdx = fromIntegral $ fromJust $ findIndex isEvent bs

    let actualEventime = dt * firstEventIdx
    return actualEventime

afterExp :: RandomGen g => g -> DTime -> b -> SF a (Event b)
afterExp g t b = SF { sfTF = tf0 }
    where
        (t', _) = randomExp g (1 / t)

        -- there can be no event at time of switching
        tf0 _ = (tfCont, NoEvent)
            where
                tfCont = afterExpAux 0 t'

        afterExpAux tCurr tEvt = SF' tf
            where
                tf dt _ 
                    | tCurr' >= tEvt = (tf', Event b)
                    | otherwise = (tf', NoEvent)
                    where
                        tCurr' = tCurr + dt
                        tf' = afterExpAux tCurr' tEvt