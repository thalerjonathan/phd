{-# LANGUAGE GADTs, Rank2Types, CPP #-}

module FrABS.SeqIteration where

import FRP.Yampa.InternalCore

import FrABS.Utils

import Data.Maybe

------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
runSeqSF :: [SF i o]
            -> ([i] -> (SF i o, i, o) -> ([i], Maybe (SF i o, i), [SF i o], [i]))
            -> SF [i] [o]
runSeqSF initSfs clbk = SF {sfTF = tf0}
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
                        (sfs', outs, ins') = runSeqInternal sfs ins clbk dt
                        -- create a continuation of this SF
                        tf' = runSeqSFAux sfs' ins'

runSeqInternal :: [SF i o]
                -> [i]
                -> ([i] -> (SF i o, i, o) -> ([i], Maybe (SF i o, i), [SF i o], [i]))
                -> DTime
                -> ([SF i o], [o], [i])
runSeqInternal sfs is clbk dt = runSeqRec sfs is [] 0 clbk dt
    where
        runSeqRec :: [SF i o]      -- the signal-functions to run sequentially in this iteration
                    -> [i]          -- the inputs for each signal-function (length must be equal!)
                    -> [i]          -- the accumulator for the new inputs
                    -> Int          -- index of the current position in the sequence
                    -> ([i] -> (SF i o, i, o) -> ([i], Maybe (SF i o, i), [SF i o], [i])) -- the callback
                    -> DTime        -- time delta since last iteration
                    -> ([SF i o], [o], [i])     -- the collection of signal-functions, outputs and inputs for the next iteration: the outputs are the output from the current iteration but the inputs are for the next one

        runSeqRec [] [] accIs _ _ _ = ([], [], accIs)
        runSeqRec (sf:sfs) (i:ins) accIns idx clbk dt
            | isJust mayCont = (sf'' : recSfs, out : recOuts, recIns)
            | otherwise = runSeqRec sfs ins' accIns' (idx + 1) clbk dt              -- NOTE: only include newInput if  isJust mayCont
            where
                -- 1. run the current signal-function with the required input (i) and time-delta (dt) and freeze it (with the time-delta)
                    -- results is a frozen signal-function (type SF) and the output of the signal-function
                (sf', out) = runAndFreezeSF sf i dt

                 -- NOTE: current input i is not included because it is replaced (or ignored)
                allIns = accIns ++ ins

                -- 2. now the callback is invoked to let the simulation handle the result of the current SF
                (allIns',           -- the new inputs, NOT including the new input of the current signal-function
                    mayCont,        -- Nothing if this signal-function should be terminated, Just (sf, newIn) if the signal-function should continue (can be replaced) with the new input
                    newSfs,         -- a list of new signal-functions to add to the system
                    newIns) =       -- a list of the inputs for the new signal-functions (above)
                        clbk allIns (sf', i, out)   -- callback is provided with a list of all the inputs (which does NOT include the current input) and
                                                        -- the frozen signal-function, old input and output of the signal-function

                -- 3. split the list of all inputs at the position we are currently at, the result are two lists:
                    -- the one infront is the input for the signal-functions in the NEXT iteration,
                    -- the one behind is the input for the signal-functions remaining in this iteration
                (accIns', ins') = splitAt idx allIns'

                (sf'', newInput) = fromJust mayCont
                accInsWithNewInput = newInput : accIns' -- TODO: this reverses the inputs!
                (recSfs, recOuts, recIns) = runSeqRec sfs ins' accInsWithNewInput (idx + 1) clbk dt

                -- TODO: add the newSfs and newIns to accumulator, but where?
------------------------------------------------------------------------------------------------------------------------