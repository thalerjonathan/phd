module WildFire.WildFireModelStatic where

import System.Random

import qualified PureAgentsPar as PA
import qualified Data.Map as Map

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
type WFTransformer = PA.AgentTransformer WFMsg WFAgentState WFEnvironment
type WFSimHandle = PA.SimHandle WFMsg WFAgentState WFEnvironment

-- NOTE: the dynamic-version of this model is not possible to implement using PAR as it would introduce
--      ordering-semantics, which are explicitly forbidden in this case utilizing types thus it can be only implemented
--      in SEQ or CONC where one is alllowed to change the global environment

burnPerTimeUnit :: Double
burnPerTimeUnit = 0.2

is :: WFAgent -> WFState -> Bool
is a wfs = (wfState s) == wfs
    where
        s = PA.state a

wfTransformer :: WFTransformer
wfTransformer (a, _, le) PA.Start = (a, le)
wfTransformer (a, ge, le) (PA.Dt dt) = (wfDt a dt, le)
wfTransformer (a, ge, le) (PA.Message (_,m)) = (wfMsg a m, le)

wfDt :: WFAgent -> Double -> WFAgent
wfDt a dt
    | is a Living = a
    | is a Dead = a
    | is a Burning = handleBurningAgent a dt

wfMsg :: WFAgent -> WFMsg -> WFAgent
wfMsg a Ignite
    | is a Living = igniteAgent a
    | otherwise = a

igniteAgent :: WFAgent -> WFAgent
igniteAgent a = PA.updateState a (\sOld -> sOld { wfState = Burning } )

handleBurningAgent :: WFAgent -> Double -> WFAgent
handleBurningAgent a dt = if burnableLeft <= 0.0 then
                            deadAgent
                            else
                                PA.updateState aAfterRandIgnite (\sOld -> sOld { rng = g' } )
    where
        b = (burnable (PA.state a))
        burnableLeft = b - (burnPerTimeUnit * dt)
        deadAgent = PA.updateState a (\sOld -> sOld { wfState = Dead, burnable = 0.0 } )
        burningAgent = PA.updateState a (\sOld -> sOld { burnable = burnableLeft } )
        (aAfterRandIgnite, g') = PA.sendMsgToRandomNeighbour burningAgent Ignite (rng (PA.state a))

wfEnvironmentFromAgents :: [WFAgent] -> PA.GlobalEnvironment WFEnvironment
wfEnvironmentFromAgents as = foldl (\accMap a -> (Map.insert (PA.agentId a) () accMap) ) Map.empty as

createRandomWFAgents :: StdGen -> (Int, Int) -> ([WFAgent], StdGen)
createRandomWFAgents gInit cells@(x, y) = (as', g')
    where
        agentCount = x*y
        (randStates, g') = createRandomStates gInit agentCount
        as = map (\idx -> PA.createAgent idx (randStates !! idx) wfTransformer) [0..agentCount-1]
        as' = map (\a -> PA.addNeighbours a (agentNeighbours a as cells) ) as


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


