{-# LANGUAGE GADTs, Rank2Types, CPP #-}

module FrABS.SeqIteration where

import FRP.Yampa
import qualified FRP.Yampa.InternalCore as YC

import FrABS.Utils

import Data.Maybe

------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
-- TODO: hide SF'
runSeqSF :: [SF i o]
            -> [i]
            -> ([i] -> (YC.SF' i o, i, o) -> ([i], i, Maybe (YC.SF' i o)))
            -> SF [i] [o]
runSeqSF initSfs initInput clbk = YC.SF {YC.sfTF = tf0}
    where
        -- NOTE: here we are at time 0 thus using initial inputs and no dt => runPar
        (nextSfs, initOs, nextIns) = runSeq initSfs initInput clbk

        tf0 = (\_ -> (tf', initOs))
        tf' = runSeqSFAux nextSfs nextIns

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runSeqSFAux sfs ins = YC.SF' tf
            where
                -- NOTE: this is a function defition
                -- tf :: DTime -> [i] -> YC.Transition [i] [o]
                tf dt _ = (tf', outs)
                    where
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', outs, ins') = runSeq' sfs ins clbk dt
                        -- create a continuation of this SF
                        tf' = runSeqSFAux sfs' ins'

runSeq :: [SF i o]
            -> [i]
            -> ([i] -> (YC.SF' i o, i, o) -> ([i], i, Maybe (YC.SF' i o)))
            -> ([YC.SF' i o], [o], [i])
runSeq sfs is clbk = runSeqRec sfs is [] 0 clbk
    where
        runSeqRec :: [SF i o]
                    -> [i]
                    -> [i]
                    -> Int
                    -> ([i] -> (YC.SF' i o, i, o) -> ([i], i, Maybe (YC.SF' i o)))
                    -> ([YC.SF' i o], [o], [i])
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

runSeq' :: [YC.SF' i o]
            -> [i]
            -> ([i] -> (YC.SF' i o, i, o) -> ([i], i, Maybe (YC.SF' i o)))
            -> DTime
            -> ([YC.SF' i o], [o], [i])
runSeq' sfs is clbk dt = runSeqRec' sfs is [] 0 clbk dt
    where
        runSeqRec' :: [YC.SF' i o]
                    -> [i]
                    -> [i]
                    -> Int
                    -> ([i] -> (YC.SF' i o, i, o) -> ([i], i, Maybe (YC.SF' i o)))
                    -> DTime
                    -> ([YC.SF' i o], [o], [i])
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
