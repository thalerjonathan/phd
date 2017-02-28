{-# LANGUAGE BangPatterns #-}

module SGModel where

import System.Random

import qualified Data.Map as Map
import Data.Maybe
import Data.List

data SGState = Defector | Cooperator deriving (Eq, Show)

type CellId = Int
type CellContainer = Map.Map CellId SGCell

data SGCell = SGCell {
    sgCellId :: CellId,
    sgCurrState :: SGState,
    sgPrevState :: SGState,
    sgLocalPayoff :: Double,
    sgCoords :: (Int, Int),
    sgNeighbourIds :: [CellId]
} deriving (Show)

bParam :: Double
bParam = 1.9

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0


stepSingle :: CellContainer -> CellContainer
stepSingle cs = cs''
        where
            cs' = Map.map (localPayoff cs) cs
            cs'' = Map.map (bestWin cs') cs'

stepSingle' :: CellContainer -> CellContainer
stepSingle' cs = Map.map ((bestWin cs) . (localPayoff cs)) cs

localPayoff :: CellContainer -> SGCell -> SGCell
localPayoff cs c = c { sgLocalPayoff = lp }
    where
        nids = sgNeighbourIds c
        lp = foldl (\payoffSum nid -> payoffSum + localPayoff' c nid cs) 0.0 nids

        localPayoff' :: SGCell -> CellId -> CellContainer -> Double
        localPayoff' c nid cs = (payoffWith c n)
            where
                n = fromJust $ Map.lookup nid cs

bestWin :: CellContainer -> SGCell -> SGCell
bestWin cs c = c { sgCurrState = currState, sgPrevState = prevState }
    where
        nids = sgNeighbourIds c
        bestCell = foldl (\best nid -> bestWin' best nid cs) c nids
        currState = sgCurrState bestCell
        prevState = sgCurrState c

        bestWin' :: SGCell -> CellId -> CellContainer -> SGCell
        bestWin' c nid cs = compareWith c n
            where
                n = fromJust $ Map.lookup nid cs

payoffWith :: SGCell -> SGCell -> Double
payoffWith cRef cOther = payoff (sgCurrState cRef) (sgCurrState cOther)

compareWith :: SGCell -> SGCell -> SGCell
compareWith cRef cBest = if (refLocalPo > bestLocalPo) then
                            cRef
                            else
                                cBest
    where
        refLocalPo = (sgLocalPayoff cRef)
        bestLocalPo = (sgLocalPayoff cBest)

payoff :: SGState -> SGState -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam

createSGCells :: (Int, Int) -> CellContainer
createSGCells cells@(x,y) = cm
    where
        cs = [ SGCell { sgCellId = (yCoord * x) + xCoord,
                        sgCurrState = Cooperator,
                        sgPrevState = Cooperator,
                        sgLocalPayoff = 0.0,
                        sgNeighbourIds = [],
                        sgCoords = (xCoord, yCoord) } | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
        cs' = map (\c -> c { sgNeighbourIds = cellNeighbourIds c cs } ) cs
        cm = foldl (\accMap c -> Map.insert (sgCellId c) c accMap ) Map.empty cs'

setDefector :: CellContainer -> (Int, Int) -> (Int, Int) -> CellContainer
setDefector cs (pX, pY) (x, y)
    | isNothing mayCellAtPos = cs
    | otherwise = Map.insert posId defectedCellAtPos cs
    where
        posId = pY * x + pX
        mayCellAtPos = Map.lookup posId cs
        cellAtPos = (fromJust mayCellAtPos)
        defectedCellAtPos = cellAtPos { sgCurrState = Defector, sgPrevState = Defector }

cellNeighbourIds :: SGCell -> [SGCell] -> [CellId]
cellNeighbourIds c cs = map sgCellId cns
    where
        neighbourCells = neighbours (sgCoords c)
        cns = filter (\c' -> any (==(sgCoords c')) neighbourCells ) cs

neighbourhood :: [(Int, Int)]
neighbourhood = [topLeft, top, topRight,
                 left, center, right,
                 bottomLeft, bottom, bottomRight]
    where
        topLeft = (-1, -1)
        top = (0, -1)
        topRight = (1, -1)
        left = (-1, 0)
        center = (0, 0)
        right = (1, 0)
        bottomLeft = (-1, 1)
        bottom = (0, 1)
        bottomRight = (1, 1)

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = map (\(x', y') -> (x+x', y+y')) neighbourhood
