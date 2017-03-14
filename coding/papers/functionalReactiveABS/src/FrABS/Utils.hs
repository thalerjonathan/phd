module FrABS.Utils where

import FRP.Yampa.InternalCore

------------------------------------------------------------------------------------------------------------------------
-- Running a Signal-Function
------------------------------------------------------------------------------------------------------------------------
runSF :: SF a b -> a -> (SF a b, b)
runSF sf0 a0 = (SF {sfTF = tf0}, b0)
    where
        (sf', b0) = runSFInit sf0 a0

        tf0 = (\_ -> (tf', b0))
        tf' = runSFAux sf'

        runSFAux sfCont  = SF' tf
            where
                tf dt i = (sf', b0)
                    where
                        (sf', b0) = runSFCont sfCont i dt

-- This will run the given SF with the given input a and returns the continuation SF with the output b
runSFInit :: SF a b -> a -> (SF' a b, b)
runSFInit sf0 a0 = (sfTF sf0) a0

-- sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
runSFCont :: SF' a b -> a -> DTime -> (SF' a b, b)
runSFCont sf0 a0 dt0 = (sfTF' sf0 dt0) a0
------------------------------------------------------------------------------------------------------------------------