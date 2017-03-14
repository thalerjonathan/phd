{-# LANGUAGE GADTs, Rank2Types, CPP #-}

module FrABS.ParIteration where

import FRP.Yampa
import qualified FRP.Yampa.InternalCore as YC

import FrABS.Utils

import Data.Maybe

------------------------------------------------------------------------------------------------------------------------
-- PAR implementation
------------------------------------------------------------------------------------------------------------------------
-- TODO: hide SF'
runParSF :: [SF i o]
            -> [i]
            -> ([i] -> [o] -> [YC.SF' i o] -> ([i], [YC.SF' i o]))
            -> SF [i] [o]
runParSF initSfs initInput clbk = YC.SF {YC.sfTF = tf0}
    where
        -- NOTE: here we are at time 0 thus using initial inputs and no dt => runPar
        (nextSfs, initOs) = runPar initSfs initInput

        tf0 = (\_ -> (tf', initOs))
        tf' = runParSFAux nextSfs initInput initOs

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runParSFAux sfs ins outs = YC.SF' tf
            where
                -- NOTE: this is a function defition
                -- tf :: DTime -> [i] -> YC.Transition [i] [o]
                tf dt _ = (tf', outs')
                    where
                        -- using the callback to create the next inputs and allow changing of the SF-collection
                        (ins', sfs') = clbk ins outs sfs
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs'', outs') = runPar' sfs' ins' dt
                        -- create a continuation of this SF
                        tf' = runParSFAux sfs'' ins' outs'

runPar :: [SF i o]
            -> [i]
            -> ([YC.SF' i o], [o])
runPar sfs oldIns = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs

        runSFHelper :: (SF i o, i)
                        -> ([YC.SF' i o], [o])
                        -> ([YC.SF' i o], [o])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFInit sf i

runPar' :: [YC.SF' i o]
            -> [i]
            -> DTime
            -> ([YC.SF' i o], [o])
runPar' sfs oldIns dt = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr (runSFHelper dt) ([], []) sfInPairs

        runSFHelper ::  DTime
                        -> (YC.SF' i o, i)
                        -> ([YC.SF' i o], [o])
                        -> ([YC.SF' i o], [o])
        runSFHelper dt (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = runSFCont sf i dt
------------------------------------------------------------------------------------------------------------------------