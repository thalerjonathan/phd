{-# LANGUAGE Arrows #-}

module Agent.YampaSeqTest where

import qualified FRP.Yampa as Yampa
import FRP.Yampa.Switches
import FRP.Yampa.InternalCore

import Data.Maybe
import Debug.Trace

type TestInput = Int
type TestOutput = Double

------------------------------------------------------------------------------------------------------------------------
-- SEQ Test
------------------------------------------------------------------------------------------------------------------------
testSeq:: IO ()
testSeq = do
            -- let (sfs', os) = Yampa.embed (runSeq sfs is testSeqCallback) (is, sts)
            let steps = 50
            let dt = 1.0
            let oss = runStepsPar sfs is steps dt
            let os = (last oss)
            mapM (putStrLn . show) os
            return ()

        where
            n = 10
            sfs = map simpleSF [0..n-1]
            is = [0..n-1]



runStepsPar :: [SF TestInput TestOutput] -> [TestInput] -> Int -> DTime -> [[TestOutput]]
runStepsPar sfs initInput 0 dt = []
runStepsPar sfs initInput steps dt = os : oss
    where
        (sfs', os, newInputs) = runPar sfs initInput testParCallback
        (_, oss) = (runStepsPar' sfs' newInputs (steps - 1) dt)

        runStepsPar' :: [SF' TestInput TestOutput] -> [TestInput] -> Int -> DTime -> ([SF' TestInput TestOutput], [[TestOutput]])
        runStepsPar' sfs is 0 dt = ([], [])
        runStepsPar' sfs is steps dt = (sfs'', os : oss)
            where
                (sfs', os, newInputs) = runPar' sfs is testParCallback dt
                (sfs'', oss) = runStepsPar' sfs' newInputs (steps - 1) dt




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



testParCallback :: [TestInput] -> [TestOutput] -> [SF' TestInput TestOutput] -> ([TestInput], [SF' TestInput TestOutput])
testParCallback oldIns newOuts allSfs = (newIns, allSfs)
    where
        newIns = testParCallback' oldIns newOuts

        testParCallback' :: [TestInput] -> [TestOutput] -> [TestInput]
        testParCallback' [] [] = []
        testParCallback' (i:is) (o:os) = newIn : testParCallback' is os
            where
                newIn = outputToNewInput i o

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

outputToNewInput :: TestInput -> TestOutput -> TestInput
outputToNewInput oldIn newOut = truncate $ realToFrac newOut

simpleSF :: Int -> SF TestInput TestOutput
simpleSF off = proc i ->
                do
                    let i' = i + off
                    returnA -< fromInteger $ toInteger i'

------------------------------------------------------------------------------------------------------------------------
-- PAR implementation
------------------------------------------------------------------------------------------------------------------------
runPar :: [SF TestInput TestOutput]
            -> [TestInput]
            -> ([TestInput] -> [TestOutput] -> [SF' TestInput TestOutput] -> ([TestInput], [SF' TestInput TestOutput]))
            -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
runPar sfs oldIns clbk = (sfs'', newOuts, newIns)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs
        (newIns, sfs'') = clbk oldIns newOuts sfs'

        runSFHelper :: (SF TestInput TestOutput, TestInput)
                        -> ([SF' TestInput TestOutput], [TestOutput])
                        -> ([SF' TestInput TestOutput], [TestOutput])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFInit sf i

runPar' :: [SF' TestInput TestOutput]
            -> [TestInput]
            -> ([TestInput] -> [TestOutput] -> [SF' TestInput TestOutput] -> ([TestInput], [SF' TestInput TestOutput]))
            -> DTime
            -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
runPar' sfs oldIns clbk dt = (sfs'', newOuts, newIns)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr (runSFHelper dt) ([], []) sfInPairs
        (newIns, sfs'') = clbk oldIns newOuts sfs'

        runSFHelper ::  DTime
                        -> (SF' TestInput TestOutput, TestInput)
                        -> ([SF' TestInput TestOutput], [TestOutput])
                        -> ([SF' TestInput TestOutput], [TestOutput])
        runSFHelper dt (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFCont sf i dt

------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
runSeq :: [SF TestInput TestOutput]
            -> [TestInput]
            -> ([TestInput] -> (SF' TestInput TestOutput, TestInput, TestOutput) -> ([TestInput], TestInput, Maybe (SF' TestInput TestOutput)))
            -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
runSeq sfs is clbk = runSeqRec sfs is [] 0 clbk
    where
        runSeqRec :: [SF TestInput TestOutput]
                    -> [TestInput]
                    -> [TestInput]
                    -> Int
                    -> ([TestInput] -> (SF' TestInput TestOutput, TestInput, TestOutput) -> ([TestInput], TestInput, Maybe (SF' TestInput TestOutput)))
                    -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
        runSeqRec [] [] accIs _ _ = ([], [], accIs)
        runSeqRec (sf:sfs) (oldIn:is) accIs idx clbk
            | isJust mayCont = (sf' : recSfs, newOut : recOs, recIs)
            | otherwise = runSeqRec sfs is' accIs' (idx + 1) clbk                 -- NOTE: only include newInput if  isJust mayCont
            where
                (sf', newOut) = runSFInit sf oldIn

                allIs = accIs ++ is -- NOTE: current input is not included because it is thrown away anyway
                (allIs', newIn, mayCont) = clbk allIs (sf', oldIn, newOut)
                (accIs', is') = splitAt idx allIs'
                accIsWithNewInput = newIn : accIs'
                (recSfs, recOs, recIs) = runSeqRec sfs is' accIsWithNewInput (idx + 1) clbk

runSeq' :: [SF' TestInput TestOutput]
            -> [TestInput]
            -> ([TestInput] -> (SF' TestInput TestOutput, TestInput, TestOutput) -> ([TestInput], TestInput, Maybe (SF' TestInput TestOutput)))
            -> DTime
            -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
runSeq' sfs is clbk dt = runSeqRec' sfs is [] 0 clbk dt
    where
        runSeqRec' :: [SF' TestInput TestOutput]
                    -> [TestInput]
                    -> [TestInput]
                    -> Int
                    -> ([TestInput] -> (SF' TestInput TestOutput, TestInput, TestOutput) -> ([TestInput], TestInput, Maybe (SF' TestInput TestOutput)))
                    -> DTime
                    -> ([SF' TestInput TestOutput], [TestOutput], [TestInput])
        runSeqRec' [] [] accIs _ _ _ = ([], [], accIs)
        runSeqRec' (sf:sfs) (i:is) accIs idx clbk dt
            | isJust mayCont = (sf' : recSfs, newOut : recOs, recIs)
            | otherwise = runSeqRec' sfs is' accIs' (idx + 1) clbk dt              -- NOTE: only include newInput if  isJust mayCont
            where
                (sf', newOut) = runSFCont sf i dt

                allIs = accIs ++ is -- NOTE: current input is not included because it is thrown away anyway
                (allIs', newInput, mayCont) = clbk allIs (sf', i, newOut)
                (accIs', is') = splitAt idx allIs'

                accIsWithNewInput = newInput : accIs'
                (recSfs, recOs, recIs) = runSeqRec' sfs is' accIsWithNewInput (idx + 1) clbk dt
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
-- This will run the given SF with the given input a and returns the continuation SF with the output b
runSFInit :: SF a b -> a -> (SF' a b, b)
runSFInit sf0 a0 = (sfTF sf0) a0

runSFCont :: SF' a b -> a -> DTime -> (SF' a b, b)
runSFCont sf0 a0 dt0 = (sfTF' sf0 dt0) a0
------------------------------------------------------------------------------------------------------------------------