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
                let (sf', o) = runAndFreezeSF sf i 2.0
                putStrLn $ show o
                let i' = outputToNewInput i o
                let (sf'', o') = runAndFreezeSF sf i' 2.0
                putStrLn $ show o'
                return ()
------------------------------------------------------------------------------------------------------------------------
-- PAR Test
------------------------------------------------------------------------------------------------------------------------
testParEmbed:: IO ()
testParEmbed = do
                    let oos = embed (runParSF sfs testParCallback) (is, sts)
                    -- putStrLn $ show (length oos)
                    let os = (last oos)
                    mapM (putStrLn . show) os
                    return ()
        where
            n = 3
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]
            steps = 3
            dt = 1.0
            sts = replicate steps (dt, Nothing)

testParCallback :: [TestInput]
                    -> [TestOutput]
                    -> [SF TestInput TestOutput]
                    -> ([SF TestInput TestOutput], [TestInput])
testParCallback oldIns newOuts allSfs = (allSfs, newIns)
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
testSeqEmbed:: IO ()
testSeqEmbed = do
                    let oos = embed (runSeqSF sfs testSeqCallback) (is, sts)
                    -- putStrLn $ show (length oos)
                    let os = (last oos)
                    mapM (putStrLn . show) os
                    return ()
        where
            n = 3
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]
            steps = 3
            dt = 1.0
            sts = replicate steps (dt, Nothing)

-- NOTE: this callback feeds in all the inputs and the current working triple: SF, Inpout and Output
-- It allows to change the inputs of future SFs and may return the SF. if it doesnt return a SF this means it is deleted from the system
testSeqCallback :: [TestInput] -- the existing inputs
                    -> (SF TestInput TestOutput, TestInput, TestOutput) -- the current working triple
                    -> ([TestInput], TestInput, Maybe (SF TestInput TestOutput)) -- optionally returns a sf-continuation for the current, can return new signal-functions and changed testinputs
testSeqCallback allIs (sf, oldIn, newOut) = (allIs', newIn, maySf)
    where
        allIs' = map (\i' -> i' + (truncate $ realToFrac newOut)) allIs  -- distribute the current output to the new inputs
        newIn = outputToNewInput oldIn newOut
        maySf = Just sf
------------------------------------------------------------------------------------------------------------------------

outputToNewInput :: TestInput -> TestOutput -> TestInput
outputToNewInput oldIn newOut = truncate $ realToFrac newOut

simpleSF :: Int -> SF TestInput TestOutput
simpleSF off = proc i ->
                do
                    t <- time -< 0
                    let i' = trace ("time = " ++ (show t)) (i + off)
                    returnA -< fromInteger $ toInteger i'