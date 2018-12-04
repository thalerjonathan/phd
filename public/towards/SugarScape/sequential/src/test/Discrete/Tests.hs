module Discrete.Tests
  ( discreteTests
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Discrete.Discrete

discreteTests :: TestTree 
discreteTests = testGroup "Discrete 2D Grid Tests"
            [ testgroup_wrapDisc2d_unit
            , testgroup_wrapDisc2d_quickCheck
            , testgroup_neighbourInDistance
            ]

-------------------------------------------------------------------------------
-- TEST-GROUPS
-------------------------------------------------------------------------------
testgroup_wrapDisc2d_unit :: TestTree 
testgroup_wrapDisc2d_unit = 
    testGroup "Unit wrapDisc2d" 
        [test_wrap_clip_none,
         test_wrap_clip_max_both,
         test_wrap_clip_max_x,
         test_wrap_clip_max_y,
         test_wrap_clip_max_edge_both,
         test_wrap_clip_max_edge_x,
         test_wrap_clip_max_edge_y,
         test_wrap_clip_min_both,
         test_wrap_clip_min_x,
         test_wrap_clip_min_y,
         test_wrap_clip_min_edge_both,
         test_wrap_clip_min_edge_x,
         test_wrap_clip_min_edge_y]

testgroup_wrapDisc2d_quickCheck :: TestTree 
testgroup_wrapDisc2d_quickCheck =
    testGroup "QuickCheck wrapDisc2d" 
        [test_wrap_clip_quick,
         test_wrap_horizontal_quick,
         test_wrap_vertical_quick,
         test_wrap_both_quick]

testgroup_neighbourInDistance :: TestTree 
testgroup_neighbourInDistance =
    testGroup "neighbourInDistance"
        [test_neighboursInNeumannDistance_0,
         test_neighboursInNeumannDistance_1,
         test_neighboursInNeumannDistance_2]