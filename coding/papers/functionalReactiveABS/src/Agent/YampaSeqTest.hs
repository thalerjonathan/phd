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

testSeqEmbed:: IO ()
testSeqEmbed = do
                    let oos = Yampa.embed (runParSF sfs is testParCallback) (is, sts)
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

samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt



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
-- data SF a b = SF {sfTF :: a -> Transition a b}
-- => SF {sfTF :: a -> (SF' a b, b)}

-- type Transition a b = (SF' a b, b)
-- SF' :: !(DTime -> a -> Transition a b) -> SF' a b
-- sfTF' :: SF' a b -> (DTime -> a -> Transition a b)

-- TODO: hide SF'
runParSF :: [SF TestInput TestOutput]
            -> [TestInput]
            -> ([TestInput] -> [TestOutput] -> [SF' TestInput TestOutput] -> ([TestInput], [SF' TestInput TestOutput]))
            -> SF [TestInput] [TestOutput]
runParSF initSfs initInput clbk = SF {sfTF = tf0}
    where
        -- NOTE: here we are at time 0 thus using initial inputs and no dt => runPar
        (nextSfs, initOs) = runPar initSfs initInput

        tf0 = (\_ -> (tf', initOs))
        tf' = runParSFAux nextSfs initInput initOs

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runParSFAux sfs ins outs = SF' tf
            where
                -- NOTE: this is a function defition
                tf :: DTime -> [TestInput] -> Transition [TestInput] [TestOutput]
                tf dt _ = (tf', outs')
                    where
                        -- using the callback to create the next inputs and allow changing of the SF-collection
                        (ins', sfs') = clbk ins outs sfs
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs'', outs') = runPar' sfs' ins' dt
                        -- create a continuation of this SF
                        tf' = runParSFAux sfs'' ins' outs'

runPar :: [SF TestInput TestOutput]
            -> [TestInput]
            -> ([SF' TestInput TestOutput], [TestOutput])
runPar sfs oldIns = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs

        runSFHelper :: (SF TestInput TestOutput, TestInput)
                        -> ([SF' TestInput TestOutput], [TestOutput])
                        -> ([SF' TestInput TestOutput], [TestOutput])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFInit sf i

runPar' :: [SF' TestInput TestOutput]
            -> [TestInput]
            -> DTime
            -> ([SF' TestInput TestOutput], [TestOutput])
runPar' sfs oldIns dt = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr (runSFHelper dt) ([], []) sfInPairs

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

-- sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
runSFCont :: SF' a b -> a -> DTime -> (SF' a b, b)
runSFCont sf0 a0 dt0 = (sfTF' sf0 dt0) a0
------------------------------------------------------------------------------------------------------------------------