module WildFire.WildFireModelDynamic where

import System.Random
import Data.Maybe

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import qualified Data.Map as Map
import qualified PureAgentsAct as PA

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

type WFCellContainer = Map.Map WFCellIdx (TVar WFCell)

data WFEnvironment = WFEnvironment {
    cells :: WFCellContainer,
    cellLimits :: WFCellCoord           -- NOTE: this will stay constant
    -- TODO: add wind-direction
}

type WFAgent = PA.Agent WFMsg WFAgentState WFEnvironment
type WFTransformer = PA.AgentTransformer WFMsg WFAgentState WFEnvironment
type WFSimHandle = PA.SimHandle WFMsg WFAgentState WFEnvironment

burnPerTimeUnit :: Double
burnPerTimeUnit = 0.3

wfTransformer :: WFTransformer
wfTransformer (a, e) PA.Start = return a
wfTransformer ae (PA.Dt (t, dt)) = wfUpdtHandler ae dt
wfTransformer (a, e) (PA.Message m) = return a                            -- NOTE: in this case no messages are sent between agents

-- NOTE: an active agent is always burning: it can be understood as the process of a burning cell
wfUpdtHandler :: (WFAgent, WFEnvironment) -> Double -> STM WFAgent
wfUpdtHandler ae@(a, _) dt = do
                                hasBurnedDown <- burnDown ae dt
                                if hasBurnedDown then
                                    killCellAndAgent ae
                                    else
                                        igniteRandomNeighbour ae


igniteRandomNeighbour :: (WFAgent, WFEnvironment) -> STM WFAgent
igniteRandomNeighbour ae@(a, e) = do
                                    cell <- readTVar cVar
                                    let (randCoord, g') = randomNeighbourCoord g (coord cell)
                                    let a' = PA.updateState a (\sOld -> sOld { rng = g' } )

                                    let randCellVarMaybe = cellByCoord e randCoord

                                    maybe (return a') (igniteValidCell a) randCellVarMaybe

    where
        cVar = cellOfAgent ae
        g = (rng (PA.state a))

        igniteValidCell :: WFAgent -> TVar WFCell -> STM WFAgent
        igniteValidCell a cVar = do
                                    isLiving <- isLiving cVar
                                    if isLiving then
                                        igniteLivingCell a cVar
                                        else
                                            return a

        igniteLivingCell :: WFAgent -> TVar WFCell -> STM WFAgent
        igniteLivingCell a cVar = do
                                    let g = (rng (PA.state a))
                                    (aNew, g') <- igniteCell g cVar
                                    let a' = PA.updateState a (\sOld -> sOld { rng = g' } )
                                    return (PA.newAgent a' aNew)

isLiving :: TVar WFCell -> STM Bool
isLiving cVar = do
                    cell <- readTVar cVar
                    return ((cellState cell) == Living)

burnDown :: (WFAgent, WFEnvironment) -> Double -> STM Bool
burnDown ae dt = do
                    cell <- readTVar cVar
                    let b = burnable cell
                    let burnableLeft = max (b - (burnPerTimeUnit * dt)) 0.0
                    modifyTVar cVar (\c -> c { burnable = burnableLeft })
                    return (burnableLeft <= 0.0)
    where
        cVar = cellOfAgent ae

killCellAndAgent :: (WFAgent, WFEnvironment) -> STM WFAgent
killCellAndAgent ae@(a, e) = do
                                changeCell cVar (\c -> c { burnable = 0.0, cellState = Dead } )
                                return (PA.kill a)
    where
        cVar = cellOfAgent ae


changeCell :: TVar WFCell -> (WFCell -> WFCell) -> STM ()
changeCell cVar tx = modifyTVar cVar tx

cellOfAgent :: (WFAgent, WFEnvironment) -> TVar WFCell
cellOfAgent (a, e) = fromJust maybeCell
    where
        maybeCell = Map.lookup (cidx (PA.state a)) (cells e)

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

igniteCell :: StdGen -> TVar WFCell -> STM (WFAgent, StdGen)
igniteCell g cVar = do
                        cell <- readTVar cVar
                        let aState = WFAgentState { cidx = (cellIdx cell), rng = g' }
                        let id = (cellIdx cell)
                        a <- PA.createAgent id aState wfTransformer
                        changeCell cVar (\c -> c { cellState = Burning } ) -- NOTE: don't need any neighbours because no messaging!
                        return (a, g'')
    where
        (g', g'') = split g

cellByCoord :: WFEnvironment -> WFCellCoord -> Maybe (TVar WFCell)
cellByCoord env co = Map.lookup idx cs
    where
        limits = cellLimits env
        cs = cells env
        idx = idxByCoord co limits

createEnvironment :: (Int, Int) -> STM WFEnvironment
createEnvironment mcs@(maxX, maxY) = do
                                        let cs = [ WFCell { cellIdx = (y*maxX) + x,
                                                                    coord = (x, y),
                                                                    burnable = 1.0,
                                                                    cellState = Living } | y <- [0..maxY-1], x <- [0..maxX-1] ]
                                        csVars <- mapM newTVar cs
                                        csVarsMaped <- foldl insertCell (return Map.empty) cs
                                        return WFEnvironment { cells = csVarsMaped, cellLimits = mcs }

insertCell :: STM (Map.Map WFCellIdx (TVar WFCell)) -> WFCell -> STM (Map.Map WFCellIdx (TVar WFCell))
insertCell m c = do
                    cVar <- newTVar c
                    m' <- m
                    return (Map.insert (cellIdx c) cVar m')

idxByCoord :: WFCellCoord -> (Int, Int) -> Int
idxByCoord (x, y) (maxX, maxY) = (y*maxX) + x