module FrABS.Simulation.SeqIteration where

import FrABS.Simulation.Utils

import FRP.Yampa.InternalCore

import Data.Maybe
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
runSeqSF :: [SF i o]
            -> (([i], [SF i o]) -> (SF i o) -> (SF i o, i, o) -> ([i], Maybe (SF i o, i, o)))
            -> ([o] -> ([SF i o], [i]))
            -> SF [i] [o]
runSeqSF initSfs clbkSeq clbkIter = SF {sfTF = tf0}
    where
        tf0 initInput = (tfCont, [])
            where
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk 0.0
                -- NOTE: in SEQ we need already to know the dt for the NEXT step because we are iterating in sequence => ommit first output => need 1 step more
                tfCont = runSeqSFAux initSfs initInput

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runSeqSFAux sfs ins = SF' tf
            where
                -- NOTE: this is a function definition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ = (tf', outs)
                    where
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', ins', outs) = runSeqInternal sfs ins clbkSeq clbkIter dt
                        -- create a continuation of this SF
                        tf' = runSeqSFAux sfs' ins'

-- TODO: this implementation reverts the order of the agents every iteration e.g. 1st iteration: [0,1,2], 2nd iteration: [2,1,0], 3rd iteration: [0,1,2]
runSeqInternal :: [SF i o]
                -> [i]
                -> (([i], [SF i o]) -> (SF i o) -> (SF i o, i, o) -> ([i], Maybe (SF i o, i, o)))
                -> ([o] -> ([SF i o], [i]))
                -> DTime
                -> ([SF i o], [i], [o])
runSeqInternal sfs ins clbkSeq clbkIter dt = (sfs' ++ newSfs, ins' ++ newSfsIns, outs)
    where
        (sfs', ins', outs) = runSeqRec
                                sfs
                                ins
                                0
                                clbkSeq
                                dt
                                ([], [], [])
        (newSfs, newSfsIns) = clbkIter outs

        runSeqRec :: [SF i o]               -- the signal-functions to run sequentially in this iteration
                    -> [i]                  -- the inputs for each signal-function (length must be equal!)
                    -> Int                  -- index of the current position in the sequence
                    -> (([i], [SF i o])   -- the callback for a single sequential calculation
                        -> (SF i o)
                        -> (SF i o, i, o)
                        -> ([i], Maybe (SF i o, i, o)))
                    -> DTime                -- time delta since last iteration
                    -> ([SF i o],           -- the accumulator for the next sfs
                        [i],                -- the accumulator for the next inputs
                        [o])                -- the accumulator for the all outputs (including also the ones of the killed agents)
                    -> ([SF i o], [i], [o])     -- the collection of signal-functions, outputs and inputs for the next iteration: the outputs are the output from the current iteration but the inputs are for the next one

        runSeqRec [] [] _ _ _ acc = acc
        runSeqRec (sf:sfs) (i:ins) idx clbk dt acc@(accSfs, accIns, accOuts) = runSeqRec sfs ins' idx' clbk dt acc'
            where
                -- 1. run the current signal-function with the required input (i) and time-delta (dt) and freeze it (with the time-delta)
                    -- results is a frozen signal-function (type SF) and the output of the signal-function
                (sf', out) = runAndFreezeSF sf i dt

                 -- NOTE: current input i is not included because it is replaced (or ignored)
                allIns = accIns ++ ins
                allSfs = accSfs ++ sfs

                -- 2. now the callback is invoked to let the simulation handle the result of the current SF
                (allIns',           -- the new inputs, NOT including the new input of the current signal-function
                    mayCont)        -- Nothing if this signal-function should be terminated, Just (sf, newIn) if the signal-function should continue (can be replaced) with the new input
                        = clbk (allIns, allSfs) sf (sf', i, out)   -- callback is provided with a list of all the inputs (which does NOT include the current input) and
                                                        -- the frozen signal-function, old input and output of the signal-function

                -- 3. split the list of all inputs at the position we are currently at, the result are two lists:
                    -- the one infront is the input for the signal-functions in the NEXT iteration,
                    -- the one behind is the input for the signal-functions remaining in this iteration
                (accIns', ins') = splitAt idx allIns'

                acc' = accumulateData (accSfs, accIns', accOuts) mayCont out

                idx' = if isJust mayCont then idx + 1 else idx

                accumulateData :: ([SF i o], [i], [o]) -> Maybe (SF i o, i, o) -> o -> ([SF i o], [i], [o])
                accumulateData (accSfs, accIns, accOuts) mayCont oldOut
                    | isJust mayCont = (sf : accSfs, newIn : accIns, changedOut : accOuts)
                    | otherwise = (accSfs, accIns, oldOut : accOuts)
                        where
                            (sf, newIn, changedOut) = fromJust mayCont
------------------------------------------------------------------------------------------------------------------------