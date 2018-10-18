module SugarScape.Init 
  ( createSugarScape
  ) where

import Control.Monad.Random

import Data.Char
import Data.List

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Model
import SugarScape.Random

-- the sugarscape is 51x51 in our implementation
sugarscapeDimensions :: Discrete2dCoord
sugarscapeDimensions = (51, 51)

-- taken from Iain Weaver Sugarscape implementation
-- https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
-- http://ccl.northwestern.edu/netlogo/models/community/
sugarEnvSpec :: [String]
sugarEnvSpec =
  [ "111111111111111111111111111112222222222111111111111"
  , "111111111111111111111111111222222222222222111111111"
  , "111111111111111111111111112222222222222222221111111"
  , "111111111111111111111111122222222222222222222211111"
  , "111111111111111111111111222222222222222222222221111"
  , "111110000000111111111111222222222223332222222222111"
  , "111110000000001111111111222222223333333332222222111"
  , "111110000000000111111112222222333333333333222222211"
  , "111110000000000111111112222223333333333333322222211"
  , "111110000000000011111112222223333333333333332222221"
  , "111110000000000011111122222233333344444333333222221"
  , "111110000000000111111122222233333444444433333222221"
  , "111111000000000111111122222333334444444443333222222"
  , "111111000000001111111222222333334444444443333322222"
  , "111111100000011111111222222333334444444443333322222"
  , "111111111001111111111222222333334444444443333322222"
  , "111111111111111111111222222333334444444443333222222"
  , "111111111111111111112222222333333444444433333222222"
  , "111111111111111111112222222233333344444333333222222"
  , "111111111111111111122222222233333333333333332222222"
  , "111111111111111112222222222223333333333333332222222"
  , "111111111111122222222222222223333333333333322222222"
  , "111111111122222222222222222222233333333332222222221"
  , "111111122222222222222222222222222333333222222222221"
  , "111122222222222222222222222222222222222222222222211"
  , "111222222222222222222222222222222222222222222222111"
  , "112222222222222222222222222222222222222222222221111"
  , "122222222222333333222222222222222222222222221111111"
  , "122222222233333333332222222222222222222221111111111"
  , "222222223333333333333322222222222222221111111111111"
  , "222222233333333333333322222222222211111111111111111"
  , "222222233333333333333332222222221111111111111111111"
  , "222222333333444443333332222222211111111111111111111"
  , "222222333334444444333333222222211111111111111111111"
  , "222222333344444444433333222222111111111111111111111"
  , "222223333344444444433333222222111111111100111111111"
  , "222223333344444444433333222222111111110000001111111"
  , "222223333344444444433333222222111111100000000111111"
  , "222222333344444444433333222221111111000000000111111"
  , "122222333334444444333332222221111111000000000011111"
  , "122222333333444443333332222221111110000000000011111"
  , "122222233333333333333322222211111110000000000011111"
  , "112222223333333333333322222211111111000000000011111"
  , "112222222333333333333222222211111111000000000011111"
  , "111222222233333333322222222111111111100000000011111"
  , "111222222222233322222222222111111111111000000011111"
  , "111122222222222222222222222111111111111111111111111"
  , "111112222222222222222222221111111111111111111111111"
  , "111111122222222222222222211111111111111111111111111"
  , "111111111222222222222222111111111111111111111111111"
  , "111111111111222222222211111111111111111111111111111"
  ]

createSugarScape :: RandomGen g
                 => SugarScapeParams
                 -> Rand g ([(AgentId, SugAgent g)], SugEnvironment)
createSugarScape params = do
  let agentCount = sgAgentCount params
      agentDistr = sgAgentDistribution params
      ais        = [1..agentCount]

  let coordDims  = case agentDistr of
                    Scatter      -> sugarscapeDimensions
                    (Corner dim) -> dim

  randCoords <- randomCoords (0,0) coordDims agentCount
  ras        <- mapM (\(aid, coord) -> randomAgent params (aid, coord) (sugAgent params) id) (zip ais randCoords)

  let as          = map (\(aid, (adef, _)) -> (aid, adBeh adef)) (zip ais ras)
      occupations = map (\(ad, s) -> (sugAgCoord s, (adId ad, s))) ras
      sugSpecs    = parseSugarSpec sugarEnvSpec
      sugCoords   = sugarSpecToCoords sugSpecs sugarscapeDimensions
      cells       = createCells sugCoords occupations
      env         = createDiscrete2d
                      sugarscapeDimensions
                      neumann
                      WrapBoth
                      cells

  return (as, env)

sugarSpecToCoords :: [[Int]]
                  -> Discrete2dCoord
                  -> [(Discrete2dCoord, Int)]
sugarSpecToCoords specs (dimX, dimY) 
    | length specs /= dimY = error ("sugar row count does not match y-dimensions: " ++ show (length specs) ++ " not " ++ show dimY)
    | otherwise            = sugarSpecLines specs 0 []
  where
    sugarSpecLines :: [[Int]]
                   -> Int
                   -> [(Discrete2dCoord, Int)]
                   -> [(Discrete2dCoord, Int)]
    sugarSpecLines [] _ acc       = acc
    sugarSpecLines (l : ls) y acc 
        | length l /= dimX = error ("sugar spec line size does not match x-dimensions: " ++ show (length l) ++ " not " ++ show dimX)
        | otherwise        = sugarSpecLines ls (y + 1) acc'
      where
        acc' = sugarSpecLineToCoords l 0 acc

        sugarSpecLineToCoords :: [Int]
                              -> Int
                              -> [(Discrete2dCoord, Int)]
                              -> [(Discrete2dCoord, Int)]
        sugarSpecLineToCoords [] _ accLine       = accLine
        sugarSpecLineToCoords (s : ss) x accLine = sugarSpecLineToCoords ss (x + 1) accLine'
          where
            accLine' = ((x, y), s) : accLine

parseSugarSpec :: [String]
               -> [[Int]]
parseSugarSpec = map parseSugarSpecLine
  where
    parseSugarSpecLine :: String 
                       -> [Int]
    parseSugarSpecLine line0 = reverse $ parseSugarSpecAux line0 []
      where
        parseSugarSpecAux :: String 
                          -> [Int]
                          -> [Int]
        parseSugarSpecAux [] acc = reverse acc
        parseSugarSpecAux (c : cs) acc 
          | isNumber c = parseSugarSpecAux cs (digitToInt c : acc)
          | otherwise  = error "bad character in sugar specification"

createCells :: [(Discrete2dCoord, Int)]
            -> [(Discrete2dCoord, (AgentId, SugAgentState))]
            -> [(Discrete2dCoord, SugEnvCell)]
createCells cellSpecs occupations 
  = map (initRandomCell occupations) cellSpecs
 
initRandomCell :: [(Discrete2dCoord, (AgentId, SugAgentState))] 
               -> (Discrete2dCoord, Int) 
               -> (Discrete2dCoord, SugEnvCell)
initRandomCell os (coord, sugar) = (coord, c)
  where
    mayOccupier = Data.List.find ((==coord) . fst) os
    occ         = maybe Nothing (\(_, (aid, s)) -> (Just (cellOccupier aid s))) mayOccupier

    c = SugEnvCell {
      sugEnvSugarCapacity = fromIntegral sugar
    , sugEnvSugarLevel    = fromIntegral sugar
    , sugEnvOccupier      = occ
    }

randomCoords :: RandomGen g
             => Discrete2dDimension 
             -> Discrete2dDimension 
             -> Int 
             -> Rand g [Discrete2dCoord]
randomCoords (minX, minY) (maxX, maxY) n
    | n > maxCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
    | otherwise        = do
      let coords = [ (x, y) | x <- [minX..maxX-1], y <- [minY..maxY-1] ]
      shuffCoords <- fisherYatesShuffleM coords
      return $ take n shuffCoords
  where
    maxCoords = (maxX - minX) * (maxY - minY)

{-
-- NOTE: will draw random-coords within (0,0) and limits WITHOUT repeating any coordinate
-- TODO: refine, problem is when number to draw = max elements, could take long
--      instead: generate all coords, shuffle them and take n elements 
randomCoords :: RandomGen g
             => Discrete2dDimension 
             -> Discrete2dDimension 
             -> Int 
             -> Rand g [Discrete2dCoord]
randomCoords (minX, minY) (maxX, maxY) n0
    | n0 > totalCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
    | otherwise = drawRandomCoordsAux n0 []
  where
    totalCoords = (maxX - minX) * (maxY - minY)

    drawRandomCoordsAux :: RandomGen g
                        => Int 
                        -> [Discrete2dCoord] 
                        -> Rand g [Discrete2dCoord]
    drawRandomCoordsAux 0 acc = return acc
    drawRandomCoordsAux n acc = do
      randX <- getRandomR (minX, maxX - 1)
      randY <- getRandomR (minY, maxY - 1)

      let c = (randX, randY)
      if c `elem` acc
        then drawRandomCoordsAux n acc
        else drawRandomCoordsAux (n-1) (c : acc)


addSugar :: [(Discrete2dCoord, SugEnvCell)] 
         -> [(Discrete2dCoord, SugEnvCell)]
addSugar cells = cellsWithSugarLevel4
  where
    cellsWithSugarLevel1 = initSugar cells (circlesSugar 1 [((35, 35), 20.0), ((15, 15), 20.0)])
    cellsWithSugarLevel2 = initSugar cellsWithSugarLevel1 (circlesSugar 2 [((35, 35), 15.0), ((15, 15), 15.0)])
    cellsWithSugarLevel3 = initSugar cellsWithSugarLevel2 (circlesSugar 3 [((35, 35), 10.0), ((15, 15), 10.0)])
    cellsWithSugarLevel4 = initSugar cellsWithSugarLevel3 (circlesSugar 4 [((35, 35), 5.0), ((15, 15), 5.0)])

initSugar :: [(Discrete2dCoord, SugEnvCell)]
          -> ((Discrete2dCoord, SugEnvCell) -> Double)
          -> [(Discrete2dCoord, SugEnvCell)]
initSugar cs sugarFunc = map initSugarAux cs
  where
    initSugarAux :: (Discrete2dCoord, SugEnvCell)
                 -> (Discrete2dCoord, SugEnvCell)
    initSugarAux cp@(coord, cell) = (coord, cell')
      where
        sugar = sugarFunc cp
        cell' = cell { sugEnvSugarLevel = sugar
                     , sugEnvSugarCapacity = sugar }

circlesSugar :: Double 
             -> [(Discrete2dCoord, Double)] 
             -> (Discrete2dCoord, SugEnvCell) 
             -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise    = sugEnvSugarLevel cell -- NOTE: keep the level of before
  where
    withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles
    -}