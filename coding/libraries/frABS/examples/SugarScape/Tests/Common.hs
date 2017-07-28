module Common (
    test_selectBestCells_group,
    test_culturalComputation_group
  ) where 

import FRP.FrABS

import SugarScape.Model
import SugarScape.Common

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

-------------------------------------------------------------------------------
-- cultural computation
-------------------------------------------------------------------------------
test_culturalComputation_group =
    testGroup "flipCulturalTag"
        [test_calculateTribe]

test_calculateTribe = QC.testProperty "flipCulturalTag " $ test_calculateTribeAux
test_calculateTribeAux tagActive tagPassive i = 
    -- TODO: QuickCheck is giving up because pre-conditions too strong, need to generate test-data somehow different
    (length tagActive == length tagPassive && i >= 0 && i < length tagActive) ==>
        flipCulturalTag tagActive tagPassive i == tagRequiredResult

    where
        tagPassiveFront = take i tagPassive 
        tagPassiveBack = drop (i+1) tagPassive
        tagActiveAtIdx = tagActive !! i
        tagRequiredResult = tagPassiveFront ++ [tagActiveAtIdx] ++ tagPassiveBack
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- selectBestCells
-------------------------------------------------------------------------------
{- Assuming the following cells layout with X is the reference coord
(0,0) (1,0) (1,1)
(0,1)   X   (2,1)
(0,2) (1,2) (2,2)
-}
-------------------------------------------------------------------------------
test_selectBestCells_group = 
    testGroup "selectBestCells bestSugarLevel" 
        [test_selectBestCells_bestSugarLevel_empty,
         test_selectBestCells_bestSugarLevel_singlebestsamedist,
         test_selectBestCells_bestSugarLevel_multibestsamedist,
         test_selectBestCells_bestSugarLevel_singlebestdiffdist,
         test_selectBestCells_bestSugarLevel_allbestsamedist]

test_selectBestCells_bestSugarLevel_empty = testCase "empty cells" $
    do
        let bcs = selectBestCells bestMeasureSugarLevel (0,0) []
        assertBool "cells should be empty" (null bcs)

test_selectBestCells_bestSugarLevel_singlebestsamedist = testCase "single best same distances" $
    do
        let cells = [((1,0), pc { sugEnvSugarLevel = 0 }),
                     ((0,1), pc { sugEnvSugarLevel = 1 }),
                     ((2,1), pc { sugEnvSugarLevel = 2 }),
                     ((1,2), pc { sugEnvSugarLevel = 3 })]

        let bcs = selectBestCells bestMeasureSugarLevel (1,1) cells
        assertEqual "should return only 1 cell" 1 (length bcs)

        let (bcCoord, bc) = head bcs
        assertEqual "expected different cell" (1,2) bcCoord
        assertEqual "level does not match original level" 3 (sugEnvSugarLevel bc)

test_selectBestCells_bestSugarLevel_multibestsamedist = testCase "multi best same distances" $
    do
        let cells = [((1,0), pc { sugEnvSugarLevel = 4 }),
                     ((0,1), pc { sugEnvSugarLevel = 1 }),
                     ((2,1), pc { sugEnvSugarLevel = 2 }),
                     ((1,2), pc { sugEnvSugarLevel = 4 })]

        let bcs = selectBestCells bestMeasureSugarLevel (1,1) cells
        assertEqual "should return 2 cells" 2 (length bcs)

        let ((bc1Coord, bc1):(bc2Coord, bc2):_) = bcs

        assertEqual "expected different cell 1" (1,0) bc1Coord
        assertEqual "level does not match original level of cell 1" 4 (sugEnvSugarLevel bc1)

        assertEqual "expected different cell 2" (1,2) bc2Coord
        assertEqual "level does not match original level of cell 2" 4 (sugEnvSugarLevel bc2)

        assertEqual "sugarlevels do not match although must be same" (sugEnvSugarLevel bc1) (sugEnvSugarLevel bc2)

test_selectBestCells_bestSugarLevel_singlebestdiffdist = testCase "single best different distances" $
    do
        let cells = [((1,10), pc { sugEnvSugarLevel = 4 }),
                     ((0,1), pc { sugEnvSugarLevel = 1 }),
                     ((2,1), pc { sugEnvSugarLevel = 2 }),
                     ((1,2), pc { sugEnvSugarLevel = 4 })]

        let bcs = selectBestCells bestMeasureSugarLevel (1,1) cells
        assertEqual "should return only 1 cell" 1 (length bcs)

        let (bcCoord, bc) = head bcs
        assertEqual "expected different cell" (1,2) bcCoord
        assertEqual "level does not match original level" 4 (sugEnvSugarLevel bc)

test_selectBestCells_bestSugarLevel_allbestsamedist = testCase "all best same distances" $
    do
        let cells = [((1,0), pc { sugEnvSugarLevel = 1 }),
                     ((0,1), pc { sugEnvSugarLevel = 1 }),
                     ((2,1), pc { sugEnvSugarLevel = 1 }),
                     ((1,2), pc { sugEnvSugarLevel = 1 })]

        let bcs = selectBestCells bestMeasureSugarLevel (1,1) cells
        assertEqual "should return 4 cells" 4 (length bcs)

        let ((bc1Coord, bc1):(bc2Coord, bc2):(bc3Coord, bc3):(bc4Coord, bc4):_) = bcs

        assertEqual "expected different cell 1" (1,0) bc1Coord
        assertEqual "level does not match original level of cell 1" 1 (sugEnvSugarLevel bc1)

        assertEqual "expected different cell 2" (0,1) bc2Coord
        assertEqual "level does not match original level of cell 2" 1 (sugEnvSugarLevel bc2)

        assertEqual "expected different cell 3" (2,1) bc3Coord
        assertEqual "level does not match original level of cell 1" 1 (sugEnvSugarLevel bc3)

        assertEqual "expected different cell 4" (1,2) bc4Coord
        assertEqual "level does not match original level of cell 2" 1 (sugEnvSugarLevel bc4)

        assertEqual "sugarlevels do not match although must be same" (sugEnvSugarLevel bc1) (sugEnvSugarLevel bc2)
        assertEqual "sugarlevels do not match although must be same" (sugEnvSugarLevel bc2) (sugEnvSugarLevel bc3)
        assertEqual "sugarlevels do not match although must be same" (sugEnvSugarLevel bc3) (sugEnvSugarLevel bc4)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------
-- a proto-cell
pc = SugarScapeEnvCell {
    sugEnvSugarCapacity = 0,
    sugEnvSugarLevel = 0,

    sugEnvSpiceCapacity = 0,
    sugEnvSpiceLevel = 0,

    sugEnvPolutionLevel = 0,
    sugEnvOccupier = Nothing
}
-------------------------------------------------------------------------------