{-# LANGUAGE Arrows #-}

module FrABS.YampaSeqTest where

import FrABS.Utils
import FrABS.SeqIteration
import FrABS.ParIteration

import FRP.Yampa
import FRP.Yampa.InternalCore
import Data.Maybe
import Debug.Trace

type TestInput = Int
type TestOutput = Double

testRunSF :: IO ()
testRunSF = do
                let sf = simpleSF 42
                let i = 1 :: TestInput
                let (sf', o) = runSF sf i
                putStrLn $ show o
                let i' = outputToNewInput i o
                let (sf'', o') = runSF sf i'
                putStrLn $ show o'
                return ()
------------------------------------------------------------------------------------------------------------------------
-- PAR Test
------------------------------------------------------------------------------------------------------------------------
testPar :: IO ()
testPar = do
            let steps = 1
            let dt = 1.0
            let oss = runStepsPar sfs is steps dt
            let os = (last oss)
            mapM (putStrLn . show) os
            return ()

        where
            n = 10
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]

testParEmbed:: IO ()
testParEmbed = do
                    let oos = embed (runParSF sfs is testParCallback) (is, sts)
                    -- putStrLn $ show (length oos)
                    let os = (last oos)
                    mapM (putStrLn . show) os
                    return ()
        where
            n = 10
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]
            steps = 0
            dt = 1.0
            sts = take steps $ samplingTimes 0.0 dt

runStepsPar :: [SF TestInput TestOutput] -> [TestInput] -> Int -> DTime -> [[TestOutput]]
runStepsPar sfs initInput 0 dt = []
runStepsPar sfs initInput steps dt = os : oss
    where
        (sfs', os) = runPar sfs initInput
        (newInputs, sfs'') = testParCallback initInput os sfs'
        (_, oss) = (runStepsPar' sfs'' newInputs (steps - 1) dt)

        runStepsPar' :: [SF' TestInput TestOutput] -> [TestInput] -> Int -> DTime -> ([SF' TestInput TestOutput], [[TestOutput]])
        runStepsPar' sfs is 0 dt = ([], [])
        runStepsPar' sfs is steps dt = (sfs3, os : oss)
            where
                (sfs', os) = runPar' sfs is dt
                (newInputs, sfs'') = testParCallback is os sfs'
                (sfs3, oss) = runStepsPar' sfs'' newInputs (steps - 1) dt

testParCallback :: [TestInput] -> [TestOutput] -> [SF' TestInput TestOutput] -> ([TestInput], [SF' TestInput TestOutput])
testParCallback oldIns newOuts allSfs = (newIns, allSfs)
    where
        newIns = testParCallback' oldIns newOuts

        testParCallback' :: [TestInput] -> [TestOutput] -> [TestInput]
        testParCallback' [] [] = []
        testParCallback' (i:is) (o:os) = newIn : testParCallback' is os
            where
                newIn = outputToNewInput i o
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- SEQ Test
------------------------------------------------------------------------------------------------------------------------
testSeq:: IO ()
testSeq = do
            let steps = 3
            let dt = 1.0
            let oss = runStepsSeq sfs is steps dt
            let os = (last oss)
            mapM (putStrLn . show) os
            return ()

        where
            n = 10
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]

testSeqEmbed:: IO ()
testSeqEmbed = do
                    let oos = embed (runSeqSF sfs is testSeqCallback) (is, sts)
                    -- putStrLn $ show (length oos)
                    let os = (last oos)
                    mapM (putStrLn . show) os
                    return ()
        where
            n = 10
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]
            steps = 2
            dt = 1.0
            sts = take steps $ samplingTimes 0.0 dt

runStepsSeq :: [SF TestInput TestOutput] -> [TestInput] -> Int -> DTime -> [[TestOutput]]
runStepsSeq sfs initInput 0 dt = []
runStepsSeq sfs initInput steps dt = os : oss
    where
        (sfs', os, newInputs) = runSeq sfs initInput testSeqCallback
        (_, oss) = (runStepsSeq' sfs' newInputs (steps - 1) dt)

        runStepsSeq' :: [SF' TestInput TestOutput] -> [TestInput] -> Int -> DTime -> ([SF' TestInput TestOutput], [[TestOutput]])
        runStepsSeq' sfs is 0 dt = ([], [])
        runStepsSeq' sfs is steps dt = (sfs'', os : oss)
            where
                (sfs', os, newInputs) = runSeq' sfs is testSeqCallback dt
                (sfs'', oss) = runStepsSeq' sfs' newInputs (steps - 1) dt

-- NOTE: this callback feeds in all the inputs and the current working triple: SF, Inpout and Output
-- It allows to change the inputs of future SFs and may return the SF. if it doesnt return a SF this means it is deleted from the system
testSeqCallback :: [TestInput] -- the existing inputs
                    -> (SF' TestInput TestOutput, TestInput, TestOutput) -- the current working triple
                    -> ([TestInput], TestInput, Maybe (SF' TestInput TestOutput)) -- optionally returns a sf-continuation for the current, can return new signal-functions and changed testinputs
testSeqCallback allIs (sf, oldIn, newOut) = (allIs', newIn, maySf)
    where
        allIs' = map (\i' -> i' + (truncate $ realToFrac newOut)) allIs  -- distribute the current output to the new inputs
        newIn = outputToNewInput oldIn newOut
        maySf = Just sf
------------------------------------------------------------------------------------------------------------------------


samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt

outputToNewInput :: TestInput -> TestOutput -> TestInput
outputToNewInput oldIn newOut = truncate $ realToFrac newOut

simpleSF :: Int -> SF TestInput TestOutput
simpleSF off = proc i ->
                do
                    let i' = i + off
                    returnA -< fromInteger $ toInteger i'


