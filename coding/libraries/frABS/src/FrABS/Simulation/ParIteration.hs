module FrABS.Simulation.ParIteration where

import FrABS.Simulation.Utils

import FRP.Yampa.InternalCore

------------------------------------------------------------------------------------------------------------------------
-- PAR implementation
------------------------------------------------------------------------------------------------------------------------
runParSF :: [SF i o]
            -> ([i] -> [o] -> [SF i o] -> ([SF i o], [i]))
            -> SF [i] [o]
runParSF initSfs clbk = SF {sfTF = tf0}
    where
        tf0 initInput = (tfCont, initOs)
            where
                (nextSfs, initOs) = runParInternal initSfs initInput
                tfCont = runParSFAux nextSfs initInput initOs

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runParSFAux sfs ins outs = SF' tf
            where
                -- NOTE: this is a function defition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ =  (tf', outs')
                    where
                        -- freezing the collection of SF' to 'promote' them back to SF
                        frozenSfs = freezeCol sfs dt

                        -- using the callback to create the next inputs and allow changing of the SF-collection
                        (sfs', ins') = clbk ins outs frozenSfs

                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs'', outs') = runParInternal sfs' ins'
                        -- create a continuation of this SF
                        tf' = runParSFAux sfs'' ins' outs'

runParInternal :: [SF i o]
                    -> [i]
                    -> ([SF' i o], [o])
runParInternal sfs oldIns = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs

        runSFHelper :: (SF i o, i)
                        -> ([SF' i o], [o])
                        -> ([SF' i o], [o])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = (sfTF sf) i
------------------------------------------------------------------------------------------------------------------------