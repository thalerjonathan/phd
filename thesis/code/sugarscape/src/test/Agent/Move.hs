module Agent.Move
  ( test_selectBestSites_bestSugarLevel_empty
  , test_selectBestSites_bestSugarLevel_singlebestsamedist
  , test_selectBestSites_bestSugarLevel_multibestsamedist
  , test_selectBestSites_bestSugarLevel_singlebestdiffdist
  , test_selectBestSites_bestSugarLevel_allbestsamedist
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import SugarScape.Agent.Move 
import SugarScape.Core.Model

-------------------------------------------------------------------------------
-- selectBestSites
-------------------------------------------------------------------------------
{- Assuming the following cells layout with X is the reference coord
(0,0) (1,0) (1,1)
(0,1)   X   (2,1)
(0,2) (1,2) (2,2)
-}
-------------------------------------------------------------------------------

test_selectBestSites_bestSugarLevel_empty :: TestTree
test_selectBestSites_bestSugarLevel_empty = testCase "empty cells" $ do
  let bcs = selectBestSites sugarSiteMeasure (0,0) []
  assertBool "cells should be empty" (null bcs)

test_selectBestSites_bestSugarLevel_singlebestsamedist :: TestTree
test_selectBestSites_bestSugarLevel_singlebestsamedist = testCase "single best same distances" $  do
  let cells = [ ((1,0), pc { sugEnvSiteSugarLevel = 0 })
              , ((0,1), pc { sugEnvSiteSugarLevel = 1 })
              , ((2,1), pc { sugEnvSiteSugarLevel = 2 })
              , ((1,2), pc { sugEnvSiteSugarLevel = 3 }) ]

  let bcs = selectBestSites sugarSiteMeasure (1,1) cells
  assertEqual "should return only 1 cell" 1 (length bcs)

  let (bcCoord, bc) = head bcs
  assertEqual "expected different cell" (1,2) bcCoord
  assertEqual "level does not match original level" 3 (sugEnvSiteSugarLevel bc)

test_selectBestSites_bestSugarLevel_multibestsamedist :: TestTree
test_selectBestSites_bestSugarLevel_multibestsamedist = testCase "multi best same distances" $ do
  let cells = [ ((1,0), pc { sugEnvSiteSugarLevel = 4 })
              , ((0,1), pc { sugEnvSiteSugarLevel = 1 })
              , ((2,1), pc { sugEnvSiteSugarLevel = 2 })
              , ((1,2), pc { sugEnvSiteSugarLevel = 4 }) ]

  let bcs = selectBestSites sugarSiteMeasure (1,1) cells
  assertEqual "should return 2 cells" 2 (length bcs)

  let ((bc1Coord, bc1):(bc2Coord, bc2):_) = bcs

  assertEqual "expected different cell 1" (1,0) bc1Coord
  assertEqual "level does not match original level of cell 1" 4 (sugEnvSiteSugarLevel bc1)

  assertEqual "expected different cell 2" (1,2) bc2Coord
  assertEqual "level does not match original level of cell 2" 4 (sugEnvSiteSugarLevel bc2)

  assertEqual "sugarlevels do not match although must be same" (sugEnvSiteSugarLevel bc1) (sugEnvSiteSugarLevel bc2)

test_selectBestSites_bestSugarLevel_singlebestdiffdist :: TestTree
test_selectBestSites_bestSugarLevel_singlebestdiffdist = testCase "single best different distances" $ do
  let cells = [ ((1,10), pc { sugEnvSiteSugarLevel = 4 })
              , ((0,1), pc { sugEnvSiteSugarLevel = 1 })
              , ((2,1), pc { sugEnvSiteSugarLevel = 2 })
              , ((1,2), pc { sugEnvSiteSugarLevel = 4 }) ]

  let bcs = selectBestSites sugarSiteMeasure (1,1) cells
  assertEqual "should return only 1 cell" 1 (length bcs)

  let (bcCoord, bc) = head bcs
  assertEqual "expected different cell" (1,2) bcCoord
  assertEqual "level does not match original level" 4 (sugEnvSiteSugarLevel bc)

test_selectBestSites_bestSugarLevel_allbestsamedist :: TestTree
test_selectBestSites_bestSugarLevel_allbestsamedist = testCase "all best same distances" $ do
  let cells = [ ((1,0), pc { sugEnvSiteSugarLevel = 1 })
              , ((0,1), pc { sugEnvSiteSugarLevel = 1 })
              , ((2,1), pc { sugEnvSiteSugarLevel = 1 })
              , ((1,2), pc { sugEnvSiteSugarLevel = 1 }) ]

  let bcs = selectBestSites sugarSiteMeasure (1,1) cells
  assertEqual "should return 4 cells" 4 (length bcs)

  let ((bc1Coord, bc1):(bc2Coord, bc2):(bc3Coord, bc3):(bc4Coord, bc4):_) = bcs

  assertEqual "expected different cell 1" (1,0) bc1Coord
  assertEqual "level does not match original level of cell 1" 1 (sugEnvSiteSugarLevel bc1)

  assertEqual "expected different cell 2" (0,1) bc2Coord
  assertEqual "level does not match original level of cell 2" 1 (sugEnvSiteSugarLevel bc2)

  assertEqual "expected different cell 3" (2,1) bc3Coord
  assertEqual "level does not match original level of cell 1" 1 (sugEnvSiteSugarLevel bc3)

  assertEqual "expected different cell 4" (1,2) bc4Coord
  assertEqual "level does not match original level of cell 2" 1 (sugEnvSiteSugarLevel bc4)

  assertEqual "sugarlevels do not match although must be same" (sugEnvSiteSugarLevel bc1) (sugEnvSiteSugarLevel bc2)
  assertEqual "sugarlevels do not match although must be same" (sugEnvSiteSugarLevel bc2) (sugEnvSiteSugarLevel bc3)
  assertEqual "sugarlevels do not match although must be same" (sugEnvSiteSugarLevel bc3) (sugEnvSiteSugarLevel bc4)
-------------------------------------------------------------------------------

-- a proto-cell
pc :: SugEnvSite
pc = SugEnvSite 
  { sugEnvSiteSugarCapacity = 0
  , sugEnvSiteSugarLevel    = 0

  , sugEnvSiteSpiceCapacity = 0
  , sugEnvSiteSpiceLevel    = 0

  , sugEnvSitePolutionLevel = 0
  , sugEnvSiteOccupier      = Nothing
}
