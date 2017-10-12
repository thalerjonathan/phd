module SuperSampling 
    (
        testSuperSampleAfterExp
      , testSuperSampleOccasionally
      , superSampling
    ) where

import FRP.Yampa
import FRP.Yampa.InternalCore
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

superSampling :: Int -> SF a b -> SF a [b]
superSampling n sf0 = SF { sfTF = tf0 }
    where
        -- NOTE: no supersampling at time 0
        tf0 a0 = (tfCont, [b0])
            where
                (sf', b0) = sfTF sf0 a0
                tfCont = superSamplingAux sf'

        superSamplingAux sf' = SF' tf
            where
                tf dt a = (tf', bs)
                    where
                        (sf'', bs) = superSampleRun n dt sf' a
                        tf' = superSamplingAux sf''

        superSampleRun :: Int -> DTime -> SF' a b -> a -> (SF' a b, [b])
        superSampleRun n dt sf a 
            | n <= 1 = superSampleMulti 1 dt sf a []
            | otherwise = (sf', reverse bs)  -- NOTE: need to reverse because need to respect order, use of accumulator reverses them initially
            where
                superDt = dt / fromIntegral n
                (sf', bs) = superSampleMulti n superDt sf a []

        superSampleMulti :: Int -> DTime -> SF' a b -> a -> [b] -> (SF' a b, [b])
        superSampleMulti 0 _ sf _ acc = (sf, acc)
        superSampleMulti n dt sf a acc = superSampleMulti (n-1) dt sf' a (b:acc) 
            where
                (sf', b) = sfTF' sf dt a