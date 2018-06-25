module Init 
  (
    createSugarScape
  ) where

import Control.Monad.Random
import FRP.Chimera

import Data.List

import Agent
import Common
import Model


-- TODO: init and add an environment agent
createSugarScape :: RandomGen g
                 => Int 
                 -> Discrete2dDimension 
                 -> Rand g ([SugAgentDef g], SugEnvironment)
createSugarScape agentCount dims@(_dx, _dy) = do
  -- let hdx = floor $ fromIntegral dx * 0.5
  -- let hdy = floor $ fromIntegral dy * 0.5

  randCoords <- randomCoords (0,0) dims agentCount
  --randCoords <- randomCoords (0,0) (hdx, hdy) agentCount

  ras <- mapM (\(aid, coord) -> randomAgent (aid, coord) sugAgent id) (zip [1..agentCount] randCoords)
  let (adefs, _) = unzip ras
  let occupations = map (\(ad, s) -> (sugAgCoord s, (adId ad, s))) ras

  initRandomCells <- createCells dims occupations

  let cells' = addSpice $ addSugar initRandomCells
  
  let e = createDiscrete2d
              dims
              neumann
              WrapBoth
              cells'

  return (adefs, e)

addSugar :: [(Discrete2dCoord, SugEnvCell)] 
         -> [(Discrete2dCoord, SugEnvCell)]
addSugar cells = cellsWithSugarLevel4
  where
    cellsWithSugarLevel1 = initSugar cells (circlesSugar 1 [((35, 35), 20.0), ((15, 15), 20.0)])
    cellsWithSugarLevel2 = initSugar cellsWithSugarLevel1 (circlesSugar 2 [((35, 35), 15.0), ((15, 15), 15.0)])
    cellsWithSugarLevel3 = initSugar cellsWithSugarLevel2 (circlesSugar 3 [((35, 35), 10.0), ((15, 15), 10.0)])
    cellsWithSugarLevel4 = initSugar cellsWithSugarLevel3 (circlesSugar 4 [((35, 35), 5.0), ((15, 15), 5.0)])

addSpice :: [(Discrete2dCoord, SugEnvCell)] 
         -> [(Discrete2dCoord, SugEnvCell)]
addSpice cells 
    | _enableSpice_ = cellsWithSpiceLevel4
    | otherwise = cells
  where
    cellsWithSpiceLevel1 = initSpice cells (circlesSpice 1 [((15, 35), 20.0), ((35, 15), 20.0)])
    cellsWithSpiceLevel2 = initSpice cellsWithSpiceLevel1 (circlesSpice 2 [((15, 35), 15.0), ((35, 15), 15.0)])
    cellsWithSpiceLevel3 = initSpice cellsWithSpiceLevel2 (circlesSpice 3 [((15, 35), 10.0), ((35, 15), 10.0)])
    cellsWithSpiceLevel4 = initSpice cellsWithSpiceLevel3 (circlesSpice 4 [((15, 35), 5.0), ((35, 15), 5.0)])

initSugar :: [(Discrete2dCoord, SugEnvCell)]
          -> ((Discrete2dCoord, SugEnvCell) -> Double)
          -> [(Discrete2dCoord, SugEnvCell)]
initSugar cs sugarFunc = map (initSugarAux sugarFunc) cs
  where
    initSugarAux :: ((Discrete2dCoord, SugEnvCell) -> Double)
                 -> (Discrete2dCoord, SugEnvCell)
                 -> (Discrete2dCoord, SugEnvCell)
    initSugarAux sugarFunc cp@(coord, cell) = (coord, cell')
      where
        sugar = sugarFunc cp
        cell' = cell { sugEnvSugarLevel = sugar
                     , sugEnvSugarCapacity = sugar }

initSpice :: [(Discrete2dCoord, SugEnvCell)]
          -> ((Discrete2dCoord, SugEnvCell) -> Double)
          -> [(Discrete2dCoord, SugEnvCell)]
initSpice cs spiceFunc = map (initSpiceAux spiceFunc) cs
  where
    initSpiceAux :: ((Discrete2dCoord, SugEnvCell) -> Double)
                 -> (Discrete2dCoord, SugEnvCell)
                 -> (Discrete2dCoord, SugEnvCell)
    initSpiceAux spiceFunc cp@(coord, cell) = (coord, cell')
      where
        spice = spiceFunc cp
        cell' = cell { sugEnvSpiceLevel = spice
                      , sugEnvSpiceCapacity = spice }

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
  -- randSugarCap <- getRandomR sugarCapacityRange
  -- randSpiceCap <- getRandomR spiceCapacityRange

  let mayOccupier = Data.List.find ((==coord) . fst) os
      occupier    = maybe Nothing (\(_, (aid, s)) -> (Just (cellOccupier aid s))) mayOccupier

  let c = SugEnvCell {
    sugEnvSugarCapacity = 0
  , sugEnvSugarLevel    = 0
  , sugEnvSpiceCapacity = 0
  , sugEnvSpiceLevel    = 0
  , sugEnvPolutionLevel = 0.0
  , sugEnvOccupier      = occupier
  }

  return (coord, c)

-- NOTE: will draw random-coords within (0,0) and limits WITHOUT repeating any coordinate
randomCoords :: RandomGen g
             => Discrete2dDimension 
             -> Discrete2dDimension 
             -> Int 
             -> Rand g [Discrete2dCoord]
randomCoords lower@(minX, minY) upper@(maxX, maxY) n
    | n > totalCoords = error "Logical error: can't draw more elements from a finite set than there are elements in the set"
    | otherwise = drawRandomCoordsAux lower upper n []
  where
    totalCoords = (maxX - minX) * (maxY - minY)

    drawRandomCoordsAux :: RandomGen g
                        => Discrete2dDimension 
                        -> Discrete2dDimension 
                        -> Int 
                        -> [Discrete2dCoord] 
                        -> Rand g [Discrete2dCoord]
    drawRandomCoordsAux _ _ 0 acc = return acc
    drawRandomCoordsAux lower@(minX, minY) upper@(maxX, maxY) n acc = do
      randX <- getRandomR (minX, maxX - 1)
      randY <- getRandomR (minY, maxY - 1)

      let c = (randX, randY)
      if c `elem` acc
        then drawRandomCoordsAux lower upper n acc
        else drawRandomCoordsAux lower upper (n-1) (c : acc)

_allZeroSugar :: (Discrete2dCoord, SugEnvCell) -> Double
_allZeroSugar _ = 0.0

circlesSugar :: Double 
             -> [(Discrete2dCoord, Double)] 
             -> (Discrete2dCoord, SugEnvCell) 
             -> Double
circlesSugar sugarLevel circles (coord, cell)
    | withinRadius = sugarLevel
    | otherwise = sugEnvSugarLevel cell -- NOTE: keep the level of before
  where
    withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles

circlesSpice :: Double 
             -> [(Discrete2dCoord, Double)] 
             -> (Discrete2dCoord, SugEnvCell) 
             -> Double
circlesSpice spiceLevel circles (coord, cell)
    | withinRadius = spiceLevel
    | otherwise = sugEnvSpiceLevel cell -- NOTE: keep the level of before
  where
      withinRadius = any (\(p, r) -> distanceEuclideanDisc2d p coord <= r) circles