module WildfireModel where

import Control.Monad.STM

import System.Random
import Debug.Trace

import qualified HaskellAgents as Agent

data WFState = Living | Burning | Dead deriving (Eq, Show)
data WFMsg = Ignite

data WFAgentState = WFAgentState {
    wfState :: WFState,
    burnable :: Double,
    rng :: StdGen
} deriving (Show)

type WFEnvironment = ()
type WFAgent = Agent.Agent WFMsg WFAgentState WFEnvironment
type WFMsgHandler = Agent.MsgHandler WFMsg WFAgentState WFEnvironment
type WFUpdtHandler = Agent.UpdateHandler WFMsg WFAgentState WFEnvironment

burnPerTimeUnit :: Double
burnPerTimeUnit = 0.1

is :: WFAgent -> WFState -> Bool
is a wfs = (wfState s) == wfs
    where
        s = Agent.state a

wfMsgHandler :: WFMsgHandler
wfMsgHandler a Ignite senderId
    | is a Living = trace ("received Living Ignite in " ++ (show (Agent.agentId a)) ++ " from " ++ (show senderId) ) (return (igniteAgent a))
    | otherwise = trace ("received Non-Living Ignite in " ++ (show (Agent.agentId a)) ++ " from " ++ (show senderId) ) (return a)

wfUpdtHandler :: WFUpdtHandler
wfUpdtHandler a dt
    | is a Living = return a
    | is a Dead = return a
    | is a Burning = handleBurningAgent a dt

igniteAgent :: WFAgent -> WFAgent
igniteAgent a = Agent.updateState a (\sOld -> sOld { wfState = Burning } )

handleBurningAgent :: WFAgent -> Double -> STM WFAgent
handleBurningAgent a dt = if burnableLeft <= 0.0 then
                            return deadAgent
                            else
                                do
                                    Agent.broadcastMsg burningAgent Ignite
                                    return burningAgent
                            {-
                                do
                                    g' <- Agent.sendMsgToRandomNeighbour burningAgent Ignite (rng (Agent.state a))
                                    return (Agent.updateState burningAgent (\sOld -> sOld { rng = g' } ))
                                    -}
    where
        b = (burnable (Agent.state a))
        burnableLeft = b - (burnPerTimeUnit * dt)
        deadAgent = Agent.updateState a (\sOld -> sOld { wfState = Dead, burnable = 0.0 } )
        burningAgent = Agent.updateState a (\sOld -> sOld { burnable = burnableLeft } )

createRandomWFAgents :: StdGen -> (Int, Int) -> STM ([WFAgent], StdGen)
createRandomWFAgents gInit (x, y) = do
                                        let agentCount = x*y
                                        let (randStates, g') = createRandomStates gInit agentCount
                                        as <- mapM (\idx -> Agent.createAgent idx (randStates !! idx) wfMsgHandler wfUpdtHandler) [0..agentCount-1]
                                        let anp = Agent.agentsToNeighbourPair as
                                        -- NOTE: filter neighbours
                                        -- let as' = map (\a -> Agent.addNeighbours a (filter (\(aid, _) -> isNeighbour (Agent.agentId a) aid (x,y)) anp)) as
                                        let as' = map (\a -> Agent.addNeighbours a anp) as
                                        return (as', g')

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

calculateNeighbours :: (Int, Int) -> [(Int, Int)]
calculateNeighbours (x,y) = map (\(x', y') -> (x+x', y+y')) neighbourhood

isNeighbour :: Agent.AgentId -> Agent.AgentId -> (Int, Int) -> Bool
isNeighbour a1Id a2Id (xCells, yCells) = True -- any (==(a2x, a2y)) a1NeighbourFields
    where
        a1y = floor((fromIntegral a1Id) / (fromIntegral xCells))
        a1x = mod a1Id yCells
        a2y = floor((fromIntegral a2Id) / (fromIntegral xCells))
        a2x = mod a2Id yCells
        a1NeighbourFields = calculateNeighbours (a1x, a1y)

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