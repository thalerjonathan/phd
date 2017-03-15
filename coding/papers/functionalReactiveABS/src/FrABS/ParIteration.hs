{-# LANGUAGE GADTs, Rank2Types, CPP #-}
module FrABS.ParIteration where

import FRP.Yampa.InternalCore

import FrABS.Utils

import Data.Maybe
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- PAR implementation
------------------------------------------------------------------------------------------------------------------------
runParSF :: [SF i o]
            -> ([i] -> [o] -> [SF' i o] -> ([SF' i o], [i]))
            -> SF [i] [o]
runParSF initSfs clbk = SF {sfTF = tf0}
    where
        tf0 initInput =  (tfCont, initOs)
            where
                (nextSfs, initOs) = trace "runPar" runPar initSfs initInput
                tfCont = runParSFAux nextSfs initInput initOs clbk

-- NOTE: here we create recursively a new continuation
-- ins are the old inputs from which outs resulted, together with their sfs
runParSFAux sfs ins outs clbk = SF' tf
    where
        -- NOTE: this is a function defition
        -- tf :: DTime -> [i] -> Transition [i] [o]
        tf dt i' =  (tf', outs')
            where
                -- using the callback to create the next inputs and allow changing of the SF-collection
                (sfs', ins') = clbk ins outs sfs
                -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                (sfs'', outs') = trace "runPar'" runPar' sfs' ins' dt
                -- create a continuation of this SF
                tf' = runParSFAux sfs'' ins' outs' clbk




runPar :: [SF i o]
            -> [i]
            -> ([SF' i o], [o])
runPar sfs oldIns = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs

        runSFHelper :: (SF i o, i)
                        -> ([SF' i o], [o])
                        -> ([SF' i o], [o])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFInit sf i

runPar' :: [SF' i o]
            -> [i]
            -> DTime
            -> ([SF' i o], [o])
runPar' sfs oldIns dt = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr (runSFHelper dt) ([], []) sfInPairs

        runSFHelper ::  DTime
                        -> (SF' i o, i)
                        -> ([SF' i o], [o])
                        -> ([SF' i o], [o])
        runSFHelper dt (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFCont sf i dt
------------------------------------------------------------------------------------------------------------------------