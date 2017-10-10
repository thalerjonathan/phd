module AfterExp 
    (
      testAfterExp
    ) where

import Data.Maybe
import Data.List
import Text.Printf
import System.Random
import System.IO
import Control.Parallel.Strategies

import FRP.FrABS
import FRP.Yampa

testAfterExp :: IO ()
testAfterExp = do
    let eventTime = 1 / 5      :: DTime
    let dt = 1 / 1         :: DTime
    let reps = 10000

    let ts = parMap rpar (runAfterExp eventTime dt) [1..reps]

    let avgTs = sum ts / fromIntegral (length ts)

    print $ "event occured on average after = " 
            ++ printf "%.3f" avgTs  
            ++ ", theoretical time: " ++ show eventTime


runAfterExp :: DTime -> DTime -> Int -> DTime
runAfterExp expEventTime dt seed = actualEventime
  where
    g = mkStdGen seed
    sf = afterExp g expEventTime () -- RandomGen g => g -> DTime -> b -> SF a (Event b)

    deltas = repeat (dt, Nothing)
    bs = embed sf ((), deltas) -- SF a b -> (a, [(DTime, Maybe a)]) -> [b]

    firstEventIdx = fromIntegral $ fromJust $ findIndex isEvent bs
    actualEventime = dt * firstEventIdx