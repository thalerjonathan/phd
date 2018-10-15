module SugarScape.Init 
  ( createSugarScape
  ) where

import Control.Monad.Random

import Data.List

import SugarScape.Agent
import SugarScape.AgentMonad
import SugarScape.Common
import SugarScape.Discrete
import SugarScape.Environment
import SugarScape.Model

-- the sugarscape is 50x50 in our implementation
sugarscapeDimensions :: (Int, Int)
sugarscapeDimensions = (50, 50)

createSugarScape :: RandomGen g
                 => Int 
                 -> Bool
                 -> Rand g ([(AgentId, SugAgent g)], SugEnvironment)
createSugarScape agentCount rebirthFlag = do
  randCoords <- randomCoords (0,0) sugarscapeDimensions agentCount

  let ais = [1..agentCount]
  ras <- mapM (\(aid, coord) -> randomAgent (aid, coord) (sugAgent rebirthFlag) id) (zip ais randCoords)
  let as = map (\(aid, (adef, _)) -> (aid, adBeh adef)) (zip ais ras)
  let occupations = map (\(ad, s) -> (sugAgCoord s, (adId ad, s))) ras

  initRandomCells <- createCells sugarscapeDimensions occupations

  let cells' = addSugar initRandomCells
  
  let e = createDiscrete2d
              sugarscapeDimensions
              neumann
              WrapBoth
              cells'

  let eb = sugEnvironment sugarGrowbackUnits

  return ((0, eb) : as, e)

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

createCells :: RandomGen g
            => Discrete2dDimension
            -> [(Discrete2dCoord, (AgentId, SugAgentState))]
            -> Rand g [(Discrete2dCoord, SugEnvCell)]
createCells (maxX, maxY) occupations 
    = mapM (initRandomCell occupations) coords
  where
    coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]

initRandomCell :: RandomGen g
               => [(Discrete2dCoord, (AgentId, SugAgentState))] 
               -> Discrete2dCoord 
               -> Rand g (Discrete2dCoord, SugEnvCell)
initRandomCell os coord = do
  let mayOccupier = Data.List.find ((==coord) . fst) os
      occ         = maybe Nothing (\(_, (aid, s)) -> (Just (cellOccupier aid s))) mayOccupier

  let c = SugEnvCell {
    sugEnvSugarCapacity = 0
  , sugEnvSugarLevel    = 0
  , sugEnvOccupier      = occ
  }

  return (coord, c)

-- NOTE: will draw random-coords within (0,0) and limits WITHOUT repeating any coordinate
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

circlesSugar :: Double 
             -> [(Discrete2dCoord, Double)] 
             -> (Discrete2dCoord, SugEnvCell) 
             -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise    = sugEnvSugarLevel cell -- NOTE: keep the level of before
  where
    withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles