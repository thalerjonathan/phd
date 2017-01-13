module WildFire.WildFireModelDynamic where

import System.Random
import Debug.Trace
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import qualified PureAgentsPar as PA

type WFCellIdx = Int
type WFCellCoord = (Int, Int)
data WFCellState = Living | Burning | Dead deriving (Eq, Show)
type WFMsg = ()

data WFCell = WFCell {
    cellIdx :: WFCellIdx,
    coord :: WFCellCoord,
    burnable :: Double,
    cellState :: WFCellState
} deriving (Show)

data WFAgentState = WFAgentState {
    cidx :: WFCellIdx,
    rng :: StdGen
} deriving (Show)

type WFCellContainer = Map.Map WFCellIdx WFCell

data WFEnvironment = WFEnvironment {
    cells :: WFCellContainer,
    cellLimits :: WFCellCoord
}

type WFAgent = PA.Agent WFMsg WFAgentState WFEnvironment
type WFTransformer = PA.AgentTransformer WFMsg WFAgentState WFEnvironment
type WFSimHandle = PA.SimHandle WFMsg WFAgentState WFEnvironment

burnPerTimeUnit :: Double
burnPerTimeUnit = 0.3

wfTransformer :: WFTransformer
wfTransformer ae (_, PA.Dt dt) = a'
    where
        (a', e') = wfUpdtHandler ae dt
wfTransformer (a, e) (_, PA.Domain m) = a                            -- NOTE: in this case no messages are sent between agents

-- NOTE: an active agent is always burning: it can be understood as the process of a burning cell
wfUpdtHandler :: (WFAgent, WFEnvironment) -> Double -> (WFAgent, WFEnvironment)
wfUpdtHandler ae@(a, _) dt = if hasBurnedDown ae' then
                                killCellAndAgent ae'
                                else
                                    igniteRandomNeighbour ae'
    where
        e' = burnDown ae dt
        ae' = (a, e')

igniteRandomNeighbour :: (WFAgent, WFEnvironment) -> (WFAgent, WFEnvironment)
igniteRandomNeighbour ae@(a, e) = if isJust randCellMaybe then
                                    if ((cellState randCell) == Living) then
                                        (PA.newAgent a'' aNew, e')
                                        else
                                            (a', e)
                                    else
                                        (a', e)
    where
        c = cellOfAgent ae
        g = (rng (PA.state a))

        (randCoord, g') = randomNeighbourCoord g (coord c)
        a' = PA.updateState a (\sOld -> sOld { rng = g' } )

        randCellMaybe = cellByCoord e randCoord
        randCell = fromJust randCellMaybe

        (aNew, e', g'') = igniteCell g' randCell e

        a'' = PA.updateState a' (\sOld -> sOld { rng = g'' } )

hasBurnedDown :: (WFAgent, WFEnvironment) -> Bool
hasBurnedDown ae = (burnable c) <= 0.0
    where
        c = cellOfAgent ae

burnDown :: (WFAgent, WFEnvironment) -> Double -> WFEnvironment
burnDown ae@(a, e) dt = e'
    where
        c = cellOfAgent ae
        b = (burnable c)
        burnableLeft = max (b - (burnPerTimeUnit * dt)) 0.0
        c' = c { burnable = burnableLeft }
        e' = replaceCell e c'

cellOfAgent :: (WFAgent, WFEnvironment) -> WFCell
cellOfAgent (a, e) = fromJust maybeCell
    where
        maybeCell = Map.lookup (cidx (PA.state a)) (cells e)

killCellAndAgent :: (WFAgent, WFEnvironment) -> (WFAgent, WFEnvironment)
killCellAndAgent ae@(a, e) = (PA.kill a, e')
    where
        c = cellOfAgent ae
        c' = c { burnable = 0.0, cellState = Dead }
        e' = replaceCell e c'

neighbourhood :: [WFCellCoord]
neighbourhood = [topLeft, top, topRight,
                 left, right,
                 bottomLeft, bottom, bottomRight]
    where
        topLeft = (-1, -1)
        top = (0, -1)
        topRight = (1, -1)
        left = (-1, 0)
        right = (1, 0)
        bottomLeft = (-1, 1)
        bottom = (0, 1)
        bottomRight = (1, 1)


randomNeighbourCoord :: StdGen -> WFCellCoord -> (WFCellCoord, StdGen)
randomNeighbourCoord g (cx, cy) = (randC, g')
    where
        nsCells = map (\(nx, ny) -> (cx + nx, cy + ny) :: WFCellCoord) neighbourhood
        (randIdx, g') = randomR (0, (length nsCells) - 1) g
        randC = nsCells !! randIdx

igniteCell :: StdGen -> WFCell -> WFEnvironment -> (WFAgent, WFEnvironment, StdGen)
igniteCell g c e = (a, e', g'')
    where
        (g', g'') = split g
        aState = WFAgentState { cidx = (cellIdx c), rng = g' }
        id = (cellIdx c)
        c' = c { cellState = Burning }
        e' = replaceCell e c'
        a = PA.createAgent id aState wfTransformer -- NOTE: don't need any neighbours because no messaging!

replaceCell :: WFEnvironment -> WFCell -> WFEnvironment
replaceCell e c = e { cells = Map.insert idx c cs }
    where
        idx = cellIdx c
        cs = cells e

cellByCoord :: WFEnvironment -> WFCellCoord -> Maybe WFCell
cellByCoord env co = Map.lookup idx cs
    where
        limits = cellLimits env
        cs = cells env
        idx = idxByCoord co limits

createEnvironment :: (Int, Int) -> WFEnvironment
createEnvironment mcs@(maxX, maxY) = WFEnvironment { cells = csMaped, cellLimits = mcs }
    where
        cs = [ WFCell { cellIdx = (y*maxX) + x,
                            coord = (x, y),
                            burnable = 1.0,
                            cellState = Living } | y <- [0..maxY-1], x <- [0..maxX-1] ]
        csMaped = foldl (\acc c -> Map.insert (cellIdx c) c acc ) Map.empty cs

idxByCoord :: WFCellCoord -> (Int, Int) -> Int
idxByCoord (x, y) (maxX, maxY) = (y*maxX) + x