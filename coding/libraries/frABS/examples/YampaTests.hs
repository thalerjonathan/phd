module YampaTests 
    (
      testOccasionally
    , testAfterExp
    ) where

import Data.Maybe
import Data.List
import Text.Printf
import System.Random

import FRP.Yampa
import FRP.Yampa.InternalCore
import FRP.FrABS

testOccasionally :: IO ()
testOccasionally = do
    let eventFreq = 1 / 3       :: DTime
    let samplingFreq = 1 / 10  :: DTime
    let reps = 10000

    rs <- mapM (runOccasionally eventFreq samplingFreq 1000) [1..reps]

    let avgRs = sum rs / fromIntegral (length rs)
    print $ "average ratios = " ++ printf "%.3f" avgRs

testAfterExp :: IO ()
testAfterExp = do
    let eventTime = 15       :: DTime
    let samplingFreq = 1 / 10  :: DTime
    let reps = 1000

    ts <- mapM (runAfterExp eventTime samplingFreq) [1..reps]

    let avgTs = sum ts / fromIntegral (length ts)

    print $ "event occured on average after = " ++ show avgTs  

runOccasionally :: DTime -> DTime -> Int ->  Int -> IO Double
runOccasionally eventFreq samplingFreq steps _seed = do
    --let g = mkStdGen 0
    g <- newStdGen
    let sf = occasionally g eventFreq () -- RandomGen g => g -> Time -> b -> SF a (Event b)

    let deltas = replicate steps (samplingFreq, Nothing)
    let bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let eventCount = (length . filter isEvent) bs
    let totalCount = length bs
    let ratio = (fromIntegral eventCount / fromIntegral totalCount) :: Double
    --print (show steps ++ " steps, eventFreq = " ++ show eventFreq 
    --        ++ " samplingFreq = " ++ show samplingFreq 
    --        ++ " ratio = " ++ printf "%.3f" ratio)

    return ratio

runAfterExp :: DTime -> DTime -> Int -> IO DTime
runAfterExp expEventTime samplingFreq _seed = do
    g <- newStdGen
    --let g = mkStdGen _seed
    let sf = afterExp g expEventTime ()

    let deltas = repeat (samplingFreq, Nothing)
    let bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let firstEventIdx = fromIntegral $ fromJust $ findIndex isEvent bs

    let actualEventime = samplingFreq * firstEventIdx
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