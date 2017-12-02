module SuperSampling 
    (
        testSuperSampleAfterExp
      , testSuperSampleOccasionally
      , superSampling
    ) where

import FRP.Yampa
import System.Random
import FRP.FrABS
import Data.Maybe
import Data.List

testSuperSampleAfterExp :: IO ()
testSuperSampleAfterExp = do
    let dt = 1.0
    let expEventTime = 5
    let g = mkStdGen 42
    let superSamples = 1

    -- RandomGen g => g -> Time -> b -> SF a (Event b)
    let sf = afterExp g expEventTime () -- RandomGen g => g -> DTime -> b -> SF a (Event b)
    let ssSf = superSampling superSamples sf

    let dts = repeat (dt, Nothing)
    let bss = embed ssSf ((), dts) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let bs = concat bss
    let firstEventIdx = fromJust $ findIndex isEvent bs
    let firstEventIdxSSAdjusted = fromIntegral firstEventIdx / fromIntegral superSamples 
    let actualEventime = dt * firstEventIdxSSAdjusted

    print $ "actualEventime = " ++ show actualEventime

testSuperSampleOccasionally :: IO ()
testSuperSampleOccasionally = do
    let steps = 10
    let dt = 1.0
    let eventFreq = 1 / 5
    let g = mkStdGen 42
    let superSamples = 100

    -- RandomGen g => g -> Time -> b -> SF a (Event b)
    let sf = occasionally g eventFreq () 
    let ssSf = superSampling superSamples sf

    let dts = replicate steps (dt, Nothing)
    let bss = embed ssSf ((), dts) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    let bs = concat bss
    
    --print bss
    --print bs
    
    print $ "length bs = " ++ show (length bs)
    print $ "length bss = " ++ show (length bss)