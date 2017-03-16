{-# LANGUAGE GADTs, Rank2Types, CPP #-}

module FrABS.SeqIteration where

import FRP.Yampa.InternalCore

import FrABS.Utils

import Data.Maybe

------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
runSeqSF :: [SF i o]
            -> ([i] -> (SF i o, i, o) -> ([i], i, Maybe (SF i o)))
            -> SF [i] [o]
runSeqSF initSfs clbk = SF {sfTF = tf0}
    where
        tf0 initInput = (tfCont, [])
            where
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk
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
                        (sfs', outs, ins') = runSeqInternal' sfs ins clbk dt
                        -- create a continuation of this SF
                        tf' = runSeqSFAux sfs' ins'

runSeqInternal :: [SF i o]
                    -> [i]
                    -> ([i] -> (SF i o, i, o) -> ([i], i, Maybe (SF i o)))
                    -> ([SF i o], [o], [i])
runSeqInternal sfs is clbk = runSeqRec sfs is [] 0 clbk
    where
        runSeqRec :: [SF i o]
                    -> [i]
                    -> [i]
                    -> Int
                    -> ([i] -> (SF i o, i, o) -> ([i], i, Maybe (SF i o)))
                    -> ([SF i o], [o], [i])
        runSeqRec [] [] accIs _ _ = ([], [], accIs)
        runSeqRec (sf:sfs) (oldIn:is) accIs idx clbk
            | isJust mayCont = (sf' : recSfs, newOut : recOs, recIs)
            | otherwise = runSeqRec sfs is' accIs' (idx + 1) clbk                 -- NOTE: only include newInput if  isJust mayCont
            where
                (sf', newOut) = runAndFreezeSF sf oldIn 0.0 -- NOTE: need the next dt at this point already

                allIs = accIs ++ is -- NOTE: current input is not included because it is thrown away anyway
                (allIs', newIn, mayCont) = clbk allIs (sf', oldIn, newOut)
                (accIs', is') = splitAt idx allIs'
                accIsWithNewInput = newIn : accIs'
                (recSfs, recOs, recIs) = runSeqRec sfs is' accIsWithNewInput (idx + 1) clbk

runSeqInternal' :: [SF i o]
                -> [i]
                -> ([i] -> (SF i o, i, o) -> ([i], i, Maybe (SF i o)))
                -> DTime
                -> ([SF i o], [o], [i])
runSeqInternal' sfs is clbk dt = runSeqRec' sfs is [] 0 clbk dt
    where
        runSeqRec' :: [SF i o]
                    -> [i]
                    -> [i]
                    -> Int
                    -> ([i] -> (SF i o, i, o) -> ([i], i, Maybe (SF i o)))
                    -> DTime
                    -> ([SF i o], [o], [i])
        runSeqRec' [] [] accIs _ _ _ = ([], [], accIs)
        runSeqRec' (sf:sfs) (i:is) accIs idx clbk dt
            | isJust mayCont = (sf' : recSfs, newOut : recOs, recIs)
            | otherwise = runSeqRec' sfs is' accIs' (idx + 1) clbk dt              -- NOTE: only include newInput if  isJust mayCont
            where
                (sf', newOut) = runAndFreezeSF sf i dt

                allIs = accIs ++ is -- NOTE: current input is not included because it is thrown away anyway
                (allIs', newInput, mayCont) = clbk allIs (sf', i, newOut)
                (accIs', is') = splitAt idx allIs'

                accIsWithNewInput = newInput : accIs'
                (recSfs, recOs, recIs) = runSeqRec' sfs is' accIsWithNewInput (idx + 1) clbk dt
------------------------------------------------------------------------------------------------------------------------