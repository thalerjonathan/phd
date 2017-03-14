module Agent.Utils where

import FRP.Yampa.InternalCore

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