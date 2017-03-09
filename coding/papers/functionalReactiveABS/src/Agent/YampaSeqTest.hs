{-# LANGUAGE Arrows #-}

module Agent.YampaSeqTest where

import FRP.Yampa
import FRP.Yampa.Switches
import FRP.Yampa.InternalCore

type TestInput = Int
type TestOutput = Double

------------------------------------------------------------------------------------------------------------------------
-- SEQ Test
------------------------------------------------------------------------------------------------------------------------
testSeq:: IO ()
testSeq = do
            let os = embed (runSeq sfs is) (is, sts)
            mapM (putStrLn . show) (head os)
            return ()

        where
            n = 10
            sts = take 1 $ samplingTimes 0.0 0.0
            sfs = map simpleSF [1..n]
            is = [1..n]

samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt


simpleSF :: Int -> SF TestInput TestOutput
simpleSF off = proc i ->
                do
                    let i' = i + off
                    t <- time -< 0
                    returnA -< t

runSeq :: [SF TestInput TestOutput] -> [TestInput] -> SF [TestInput] [TestOutput]
runSeq sfs is = proc is ->
                    do
                        returnA -< []
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
{-
-- This will run the given SF with the given input a and returns the continuation SF with the output b
runSF :: SF a b -> a -> (SF' a b, b)
runSF sf0 a0 = (sf0, b0)
    where
        -- tf0 :: SF' a b, b0 :: b
        (tf0, b0) = (sfTF sf0) a0
        -}
------------------------------------------------------------------------------------------------------------------------