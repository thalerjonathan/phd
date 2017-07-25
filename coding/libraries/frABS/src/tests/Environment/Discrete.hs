module Environment.Discrete (
    test_environment_discrete_unitgroup,
    test_environment_discrete_quickgroup
    
  ) where 

import FRP.FrABS.Environment.Discrete
import FRP.FrABS.Environment.Spatial

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import System.Random

-------------------------------------------------------------------------------
-- TEST-GROUPS
-------------------------------------------------------------------------------
test_environment_discrete_unitgroup = 
    testGroup "wrapDisc2d" 
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

test_environment_discrete_quickgroup = 
    testGroup "environment_discrete_quickgroup"
        [testgroup_wrapDisc2d,
         testgroup_neighbourInDistance]

testgroup_wrapDisc2d =
    testGroup "wrapDisc2d" 
        [test_wrap_clip_quick,
         test_wrap_horizontal_quick,
         test_wrap_vertical_quick,
         test_wrap_both_quick]

testgroup_neighbourInDistance =
    testGroup "neighbourInDistance"
        [test_neighboursInNeumannDistance_0,
         test_neighboursInNeumannDistance_1,
         test_neighboursInNeumannDistance_2]
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d ClipToMax
-------------------------------------------------------------------------------
test_wrap_clip_none = testCase "wrapDisc2d clip none" $ 
    do
        let c = (10, 10)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "no wrapping should have happend - original coordinate and wrapped should be equal"
            wc c

-- MAX
test_wrap_clip_max_both = testCase "wrapDisc2d clip max both" $ 
    do
        let c = coordPosOutside
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to max border"
            wc clipBorderMax

test_wrap_clip_max_x = testCase "wrapDisc2d clip max x" $ 
    do
        let c = (fst coordPosOutside, snd coordInside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "x must have been clipped to max border"
            wc (fst clipBorderMax, snd c)

test_wrap_clip_max_y = testCase "wrapDisc2d clip max y" $ 
    do
        let c = (fst coordInside, snd coordPosOutside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "x must have been clipped to max border"
            wc (fst c, snd clipBorderMax)

test_wrap_clip_max_edge_both = testCase "wrapDisc2d clip max edge both" $ 
    do
        let c = dims
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to max border"
            wc clipBorderMax

test_wrap_clip_max_edge_x = testCase "wrapDisc2d clip max edge x" $ 
    do
        let c = (fst dims, snd coordInside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to max border"
            wc (fst clipBorderMax, snd c)

test_wrap_clip_max_edge_y = testCase "wrapDisc2d clip max edge y" $ 
    do
        let c = (fst coordInside, snd dims)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to max border"
            wc (fst c, snd clipBorderMax)

-- MIN
test_wrap_clip_min_both = testCase "wrapDisc2d clip min both" $ 
    do
        let c = coordNegOutside
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to min border"
            wc clipBorderMin

test_wrap_clip_min_x = testCase "wrapDisc2d clip min x" $ 
    do
        let c = (fst coordNegOutside, snd coordInside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "x must have been clipped to min border"
            wc (fst clipBorderMin, snd c)

test_wrap_clip_min_y = testCase "wrapDisc2d clip min y" $ 
    do
        let c = (fst coordInside, snd coordNegOutside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "x must have been clipped to min border"
            wc (fst c, snd clipBorderMin)

test_wrap_clip_min_edge_both = testCase "wrapDisc2d clip min edge both" $ 
    do
        let c = clipBorderMin
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to min border"
            wc c

test_wrap_clip_min_edge_x = testCase "wrapDisc2d clip min edge x" $ 
    do
        let c = (fst clipBorderMin, snd coordInside)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to min border"
            wc c

test_wrap_clip_min_edge_y = testCase "wrapDisc2d clip min edge y" $ 
    do
        let c = (fst coordInside, snd clipBorderMin)
        let wc = wrapDisc2d dims ClipToMax c
        assertEqual 
            "must have been clipped to min border"
            wc c

test_wrap_clip_quick = QC.testProperty "wrapDisc2d ClipToMax ends up within borders" $
    \c@(x, y) -> let (wx, wy) = wrapDisc2d dims ClipToMax c 
                 in (wx >= 0 && wx <= fst clipBorderMax) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapHorizontal
-------------------------------------------------------------------------------
test_wrap_horizontal_quick = QC.testProperty "wrapDisc2d WrapHorizontal: x ends up within borders, y stays same" $
    \c@(x, y) -> let (wx, wy) = wrapDisc2d dims WrapHorizontal c 
                 in (wx >= 0 && wx <= fst clipBorderMax) && (wy == y)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapVertical
-------------------------------------------------------------------------------
test_wrap_vertical_quick = QC.testProperty "wrapDisc2d WrapVertical: y ends up within borders, x must stay same" $
    \c@(x, y) -> let (wx, wy) = wrapDisc2d dims WrapVertical c 
                 in (wx == x) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapBoth
-------------------------------------------------------------------------------
test_wrap_both_quick = QC.testProperty "wrapDisc2d WrapBoth: x and y ends up within borders" $
    \c@(x, y) -> let (wx, wy) = wrapDisc2d dims WrapBoth c 
                 in (wx >= 0 && wx <= fst clipBorderMax) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- neighboursInNeumannDistance
-------------------------------------------------------------------------------
test_neighboursInNeumannDistance_0 = testCase "neighboursInNeumannDistance 0" $
    do
        e <- createEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
        let cs = neighboursInNeumannDistance coordInside 0 e
        assertBool "with 0 distance, empty neighbours" (null cs)

test_neighboursInNeumannDistance_1 = testCase "neighboursInNeumannDistance 1" $
    do
        e <- createEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
        let cs = neighboursInNeumannDistance coordInside 1 e
        let requiredCs = [((10,9),219),((9,10),199),((11,10),241),((10,11),221)]

        assertEqual
            "with distance of 1, its exactly the direct neumann neighbourhood without self"
            (1 * (length neumann))
            (length cs)

        assertEqual
            "computed solution differs from required solution"
            cs
            requiredCs

test_neighboursInNeumannDistance_2 = testCase "neighboursInNeumannDistance 2" $
    do
        e <- createEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
        let cs = neighboursInNeumannDistance coordInside 2 e
        let requiredCs = [((10,8),218),((8,10),178),((12,10),262),((10,12),222),((10,9),219),((9,10),199),((11,10),241),((10,11),221)]

        assertEqual
            "with distance of 2, its twice the neumann neighbourhood without self"
            (2 * (length neumann))
            (length cs)

        assertEqual
            "computed solution differs from required solution"
            cs
            requiredCs
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
createEnv n w = 
    do
        rng <- newStdGen
        return $ createDiscrete2d dims n w cells rng
    where
        cellContent = [0 .. cellCount - 1]
        coords = [ (x, y) | x <- [0..fst dims - 1], y <- [0..snd dims - 1]]
        cells = zip coords cellContent

dims = (21, 21)
cellCount = fst dims * snd dims

coordInside = (10, 10)
coordPosOutside = (30, 35)
coordNegOutside = (-15, -20)

clipBorderMax = (fst dims - 1, snd dims - 1)
clipBorderMin = (0, 0)
-------------------------------------------------------------------------------
