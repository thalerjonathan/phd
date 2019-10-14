module Discrete.Discrete 
  ( test_wrap_clip_none
  , test_wrap_clip_max_both
  , test_wrap_clip_max_x
  , test_wrap_clip_max_y
  , test_wrap_clip_max_edge_both
  , test_wrap_clip_max_edge_x
  , test_wrap_clip_max_edge_y
  , test_wrap_clip_min_both
  , test_wrap_clip_min_x
  , test_wrap_clip_min_y
  , test_wrap_clip_min_edge_both
  , test_wrap_clip_min_edge_x
  , test_wrap_clip_min_edge_y
  , test_wrap_clip_quick
  , test_wrap_horizontal_quick
  , test_wrap_vertical_quick
  , test_wrap_both_quick
  , test_neighboursInNeumannDistance_0
  , test_neighboursInNeumannDistance_1
  , test_neighboursInNeumannDistance_2
  ) where 

import SugarScape.Core.Discrete

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-------------------------------------------------------------------------------
-- wrapDisc2d ClipToMax
-------------------------------------------------------------------------------
test_wrap_clip_none :: TestTree
test_wrap_clip_none = testCase "wrapDisc2d clip none" $ do
  let c  = (10, 10)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "no wrapping should have happend - original coordinate and wrapped should be equal" wc c

-- MAX
test_wrap_clip_max_both :: TestTree
test_wrap_clip_max_both = testCase "wrapDisc2d clip max both" $ do
  let c  = coordPosOutside
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to max border" wc clipBorderMax

test_wrap_clip_max_x :: TestTree
test_wrap_clip_max_x = testCase "wrapDisc2d clip max x" $ do
  let c  = (fst coordPosOutside, snd coordInside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "x must have been clipped to max border" wc (fst clipBorderMax, snd c)

test_wrap_clip_max_y :: TestTree
test_wrap_clip_max_y = testCase "wrapDisc2d clip max y" $ do
  let c =  (fst coordInside, snd coordPosOutside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "x must have been clipped to max border" wc (fst c, snd clipBorderMax)

test_wrap_clip_max_edge_both :: TestTree
test_wrap_clip_max_edge_both = testCase "wrapDisc2d clip max edge both" $ do
  let c  = dims
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to max border" wc clipBorderMax

test_wrap_clip_max_edge_x :: TestTree
test_wrap_clip_max_edge_x = testCase "wrapDisc2d clip max edge x" $ do
  let c  = (fst dims, snd coordInside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to max border" wc (fst clipBorderMax, snd c)

test_wrap_clip_max_edge_y :: TestTree
test_wrap_clip_max_edge_y = testCase "wrapDisc2d clip max edge y" $ do
  let c  = (fst coordInside, snd dims)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to max border" wc (fst c, snd clipBorderMax)

-- MIN
test_wrap_clip_min_both :: TestTree
test_wrap_clip_min_both = testCase "wrapDisc2d clip min both" $ do
  let c  = coordNegOutside
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to min border" wc clipBorderMin

test_wrap_clip_min_x :: TestTree
test_wrap_clip_min_x = testCase "wrapDisc2d clip min x" $ do
  let c  = (fst coordNegOutside, snd coordInside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "x must have been clipped to min border" wc (fst clipBorderMin, snd c)

test_wrap_clip_min_y :: TestTree
test_wrap_clip_min_y = testCase "wrapDisc2d clip min y" $ do
  let c  = (fst coordInside, snd coordNegOutside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "x must have been clipped to min border" wc (fst c, snd clipBorderMin)

test_wrap_clip_min_edge_both :: TestTree
test_wrap_clip_min_edge_both = testCase "wrapDisc2d clip min edge both" $ do
  let c  = clipBorderMin
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to min border" wc c

test_wrap_clip_min_edge_x :: TestTree
test_wrap_clip_min_edge_x = testCase "wrapDisc2d clip min edge x" $ do
  let c  = (fst clipBorderMin, snd coordInside)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to min border" wc c

test_wrap_clip_min_edge_y :: TestTree
test_wrap_clip_min_edge_y = testCase "wrapDisc2d clip min edge y" $ do
  let c  = (fst coordInside, snd clipBorderMin)
      wc = wrapDisc2d dims ClipToMax c
  assertEqual "must have been clipped to min border" wc c

test_wrap_clip_quick :: TestTree
test_wrap_clip_quick = QC.testProperty "wrapDisc2d ClipToMax ends up within borders" $
  \c@(_, _) -> let (wx, wy) = wrapDisc2d dims ClipToMax c 
                in (wx >= 0 && wx <= fst clipBorderMax) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapHorizontal
-------------------------------------------------------------------------------
test_wrap_horizontal_quick :: TestTree
test_wrap_horizontal_quick = QC.testProperty "wrapDisc2d WrapHorizontal: x ends up within borders, y stays same" $
  \c@(_, y) -> let (wx, wy) = wrapDisc2d dims WrapHorizontal c 
                in (wx >= 0 && wx <= fst clipBorderMax) && (wy == y)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapVertical
-------------------------------------------------------------------------------
test_wrap_vertical_quick :: TestTree
test_wrap_vertical_quick = QC.testProperty "wrapDisc2d WrapVertical: y ends up within borders, x must stay same" $
  \c@(x, _) -> let (wx, wy) = wrapDisc2d dims WrapVertical c 
                in (wx == x) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- wrapDisc2d WrapBoth
-------------------------------------------------------------------------------
test_wrap_both_quick :: TestTree
test_wrap_both_quick = QC.testProperty "wrapDisc2d WrapBoth: x and y ends up within borders" $
  \c@(_, _) -> let (wx, wy) = wrapDisc2d dims WrapBoth c 
                in (wx >= 0 && wx <= fst clipBorderMax) && (wy >= 0 && wy <= snd clipBorderMax)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- neighboursInNeumannDistance
-------------------------------------------------------------------------------
test_neighboursInNeumannDistance_0 :: TestTree
test_neighboursInNeumannDistance_0 = testCase "neighboursInNeumannDistance 0" $ do
  let e  = createTestEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
      cs = neighboursInNeumannDistance coordInside 0 False e
  assertBool "with 0 distance, empty neighbours" (null cs)

test_neighboursInNeumannDistance_1 :: TestTree
test_neighboursInNeumannDistance_1 = testCase "neighboursInNeumannDistance 1" $ do
  let e  = createTestEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
      cs = neighboursInNeumannDistance coordInside 1 False e
      requiredCs = [((10,9),219),((9,10),199),((11,10),241),((10,11),221)]

  assertEqual
      "with distance of 1, its exactly the direct neumann neighbourhood without self"
      (1 * length neumann)
      (length cs)

  assertEqual
      "computed solution differs from required solution"
      cs
      requiredCs

test_neighboursInNeumannDistance_2 :: TestTree
test_neighboursInNeumannDistance_2 = testCase "neighboursInNeumannDistance 2" $ do
  let e  = createTestEnv moore ClipToMax  -- NOTE: more is used here to make clear that for this function the neighbourhood of the environment does not matter
      cs = neighboursInNeumannDistance coordInside 2 False e
      requiredCs = [((10,8),218),((8,10),178),((12,10),262),((10,12),222),((10,9),219),((9,10),199),((11,10),241),((10,11),221)]

  assertEqual
      "with distance of 2, its twice the neumann neighbourhood without self"
      (2 * length neumann)
      (length cs)

  assertEqual
      "computed solution differs from required solution"
      cs
      requiredCs
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
createTestEnv :: Discrete2dNeighbourhood
              -> EnvironmentWrapping 
              -> Discrete2d Int
createTestEnv n w = createDiscrete2d dims n w cells
  where
    cellContent = [0 .. cellCount - 1]
    coords = [ (x, y) | x <- [0..fst dims - 1], y <- [0..snd dims - 1]]
    cells = zip coords cellContent

dims :: Discrete2dCoord
dims = (21, 21)
cellCount :: Int
cellCount = uncurry (*) dims

coordInside :: Discrete2dCoord
coordInside = (10, 10)
coordPosOutside :: Discrete2dCoord
coordPosOutside = (30, 35)
coordNegOutside :: Discrete2dCoord
coordNegOutside = (-15, -20)

clipBorderMax :: Discrete2dCoord
clipBorderMax = (fst dims - 1, snd dims - 1)
clipBorderMin :: Discrete2dCoord
clipBorderMin = (0, 0)
-------------------------------------------------------------------------------
