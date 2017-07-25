module Environment.Discrete (
    test_environment_discrete_group
  ) where 

import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Spatial

import Test.Tasty
import Test.Tasty.HUnit

import System.Random

-------------------------------------------------------------------------------
-- TEST-GROUP
-------------------------------------------------------------------------------
test_environment_discrete_group = 
    testGroup "neighbourInDistance" 
        [test_neighbourInDistance_empty]
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- WRAPPING
-------------------------------------------------------------------------------
--wrapDisc2d :: Discrete2dDimension -> EnvironmentWrapping -> Discrete2dCoord -> Discrete2dCoord

test_wrap = testCase "wrapDisc2d" $ 


-------------------------------------------------------------------------------
-- NEIGHBOURHOOD-DISTANCES
-------------------------------------------------------------------------------
test_neighbourInDistance_empty = testCase "empty neighbours" $
    do
        e <- createEnv (21,21) neumannSelf ClipToMax []
        let cs = neighbourInDistance (10, 10) 1 e
        assertBool "cells should be not empty" (not $ null cs)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
createEnv d n w cs = newStdGen >>= (\rng -> return $ createDiscrete2d d n w cs rng)
-------------------------------------------------------------------------------
