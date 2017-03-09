{-# LANGUAGE Arrows #-}

module Agent.YampaSeqTest where

import qualified FRP.Yampa as Yampa
import FRP.Yampa.Switches
import FRP.Yampa.InternalCore

type TestInput = Int
type TestOutput = Double

------------------------------------------------------------------------------------------------------------------------
-- SEQ Test
------------------------------------------------------------------------------------------------------------------------
testSeq:: IO ()
testSeq = do
            let os = Yampa.embed (runSeq sfs is) (is, sts)
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
                    t <- Yampa.time -< 0
                    returnA -< t


------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
-- This will run the given SF with the given input a and returns the continuation SF with the output b
runSFInit :: SF a b -> a -> (SF' a b, b)
runSFInit sf0 a0 = (sfTF sf0) a0

runSFCont :: SF' a b -> a -> DTime -> (SF' a b, b)
runSFCont sf0 a0 dt0 = (sfTF' sf0 dt0) a0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
runSeq :: [SF TestInput TestOutput]
            -> [TestInput]
            -> [TestInput] -> (TestInput, TestOutput) -> (Maybe (SF TestInput TestOutput), [TestInput])
            -> SF [TestInput] [TestOutput]
runSeq sfs is clbk = proc is ->
                        do
                            returnA -< []

-- NOTE: this callback feeds in all the inputs and the current working triple: SF, Inpout and Output
-- It allows to change the inputs of future SFs and may return the SF. if it doesnt return a SF this means it is deleted from the system
--
runSeqCallback :: [TestInput]   -- the existing inputs
                    -> (SF TestInput TestOutput, TestInput, TestOutput) -- the current working triple
                    -> (Maybe (SF TestInput TestOutput), [SF TestInput TestOutput], [TestInput])    -- optionally returns a sf-continuation for the current, can return new signal-functions and changed testinputs
runSeqCallback allInputs (currentSF, currentInput, currentOutput) = (Just currentSF, allInputs)
------------------------------------------------------------------------------------------------------------------------