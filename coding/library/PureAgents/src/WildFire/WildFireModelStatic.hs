module WildFire.WildFireModelStatic where

import Control.Monad.STM

import System.Random
import Debug.Trace

import qualified PureAgents as PA

type WFCell = (Int, Int)
data WFState = Living | Burning | Dead deriving (Eq, Show)
data WFMsg = Ignite

data WFAgentState = WFAgentState {
    wfState :: WFState,
    burnable :: Double,
    rng :: StdGen
} deriving (Show)

type WFEnvironment = ()
type WFAgent = PA.Agent WFMsg WFAgentState WFEnvironment
type WFMsgHandler = PA.MsgHandler WFMsg WFAgentState WFEnvironment
type WFUpdtHandler = PA.UpdateHandler WFMsg WFAgentState WFEnvironment
type WFSimHandle = PA.SimHandle WFMsg WFAgentState WFEnvironment

burnPerTimeUnit :: Double
burnPerTimeUnit = 0.2

is :: WFAgent -> WFState -> Bool
is a wfs = (wfState s) == wfs
    where
        s = PA.state a

wfMsgHandler :: WFMsgHandler
wfMsgHandler a Ignite senderId
    | is a Living = return (igniteAgent a)
    | otherwise = return a

wfUpdtHandler :: WFUpdtHandler
wfUpdtHandler a dt
    | is a Living = return a
    | is a Dead = return a
    | is a Burning = handleBurningAgent a dt

igniteAgent :: WFAgent -> WFAgent
igniteAgent a = PA.updateState a (\sOld -> sOld { wfState = Burning } )

handleBurningAgent :: WFAgent -> Double -> STM WFAgent
handleBurningAgent a dt = if burnableLeft <= 0.0 then
                            return deadAgent
                            else
                                do
                                    g' <- PA.sendMsgToRandomNeighbour burningAgent Ignite (rng (PA.state a))
                                    return (PA.updateState burningAgent (\sOld -> sOld { rng = g' } ))

    where
        b = (burnable (PA.state a))
        burnableLeft = b - (burnPerTimeUnit * dt)
        deadAgent = PA.updateState a (\sOld -> sOld { wfState = Dead, burnable = 0.0 } )
        burningAgent = PA.updateState a (\sOld -> sOld { burnable = burnableLeft } )

createRandomWFAgents :: StdGen -> (Int, Int) -> STM ([WFAgent], StdGen)
createRandomWFAgents gInit cells@(x, y) = do
                                        let agentCount = x*y
                                        let (randStates, g') = createRandomStates gInit agentCount
                                        as <- mapM (\idx -> PA.createAgent idx (randStates !! idx) wfMsgHandler wfUpdtHandler) [0..agentCount-1]
                                        let as' = map (\a -> PA.addNeighbours a (agentNeighbours a as cells) ) as
                                        return (as', g')

agentNeighbours :: WFAgent -> [WFAgent] -> (Int, Int) -> [WFAgent]
agentNeighbours a as cells = filter (\a' -> any (==(agentToCell a' cells)) neighbourCells ) as
    where
        aCell = agentToCell a cells
        neighbourCells = neighbours aCell

agentToCell :: WFAgent -> (Int, Int) -> (Int, Int)
agentToCell a (xCells, yCells) = (ax, ay)
     where
        aid = PA.agentId a
        ax = mod aid yCells
        ay = floor((fromIntegral aid) / (fromIntegral xCells))

neighbourhood :: [(Int, Int)]
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

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x,y) = map (\(x', y') -> (x+x', y+y')) neighbourhood

createRandomStates :: StdGen -> Int -> ([WFAgentState], StdGen)
createRandomStates g 0 = ([], g)
createRandomStates g n = (rands, g'')
    where
      (randState, g') = randomAgentState g
      (ras, g'') = createRandomStates g' (n-1)
      rands = randState : ras

randomAgentState :: StdGen -> (WFAgentState, StdGen)
randomAgentState g = (WFAgentState{ wfState = Living, burnable = randBurnable, rng = g'' }, g')
    where
        (randBurnable, g') = randomR(1.0, 1.0) g
        (g'', _) = split g'


