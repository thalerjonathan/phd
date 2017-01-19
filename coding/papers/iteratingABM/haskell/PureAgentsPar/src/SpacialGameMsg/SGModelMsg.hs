module SpacialGameMsg.SGModelMsg where

import System.Random

import qualified PureAgentsPar as PA
import qualified Data.Map as Map

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NeighbourPayoff SGState Double | NeighbourAction SGState deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCurrState :: SGState,
    sgPrevState :: SGState,
    sgSumPayoff :: Double,
    sgMaxPayoffValue :: Double,
    sgMaxPayoffState :: SGState
} deriving (Show)

type SGEnvironment = ()
type SGAgent = PA.Agent SGMsg SGAgentState SGEnvironment
type SGTransformer = PA.AgentTransformer SGMsg SGAgentState SGEnvironment
type SGSimHandle = PA.SimHandle SGMsg SGAgentState SGEnvironment

bParam :: Double
bParam = 1.9

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0

sgTransformer :: SGTransformer
sgTransformer (a, ge, le) (_, PA.Dt dt) = (sgDt a dt, le)
sgTransformer (a, ge, le) (_, PA.Domain m) = (sgMsg a m, le)

sgMsg :: SGAgent -> SGMsg -> SGAgent
sgMsg a (NeighbourAction s) = sgActionMsg a s
sgMsg a (NeighbourPayoff s p) = sgPayoffMsg a s p

sgActionMsg :: SGAgent -> SGState -> SGAgent
sgActionMsg a s = PA.updateState a (\s -> s { sgSumPayoff = newPo })
    where
        po = sgSumPayoff (PA.state a)
        poIncrease = payoffWith a s
        newPo = po + poIncrease

sgPayoffMsg :: SGAgent -> SGState -> Double -> SGAgent
sgPayoffMsg a sg p
    | p > poMaxVal = PA.updateState a (\s -> s { sgMaxPayoffValue = p, sgMaxPayoffState = sg } )
    | otherwise = a
    where
        poMaxVal = sgMaxPayoffValue (PA.state a)
        poMaxSt = sgMaxPayoffState (PA.state a)

-- NOTE: the first state is always the owning agent
payoffWith :: SGAgent -> SGState -> Double
payoffWith a s = payoff as s
    where
        as = sgCurrState (PA.state a)

payoff :: SGState -> SGState -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam

-- TODO: is this really correct?
sgDt :: SGAgent -> Double -> SGAgent
sgDt a dt = aAfterBroad
    where
        localState = sgCurrState (PA.state a)
        localPayoff = sgSumPayoff (PA.state a)

        bestPayoff = sgMaxPayoffValue (PA.state a)
        bestState = sgMaxPayoffState (PA.state a)

        aAfterPayoff = PA.updateState a (\s -> s { sgCurrState = bestState, sgPrevState = localState } )

        aAfterAction = PA.broadcastMsgToNeighbours aAfterPayoff (NeighbourAction bestState)
        aAfterBroad = PA.broadcastMsgToNeighbours aAfterAction (NeighbourPayoff localState localPayoff)

createRandomSGAgents :: StdGen -> (Int, Int) -> Double -> ([SGAgent], StdGen)
createRandomSGAgents gInit cells@(x,y) p = (as', g')
    where
        n = x * y
        (randStates, g') = createRandomStates gInit n p
        as = map (\idx -> PA.createAgent idx (randStates !! idx) sgTransformer) [0..n-1]
        as' = map (\a -> PA.addNeighbours a (agentNeighbours a as cells) ) as

        createRandomStates :: StdGen -> Int -> Double -> ([SGAgentState], StdGen)
        createRandomStates g 0 p = ([], g)
        createRandomStates g n p = (rands, g'')
            where
              (randState, g') = randomAgentState g p
              (ras, g'') = createRandomStates g' (n-1) p
              rands = randState : ras

sgEnvironmentFromAgents :: [SGAgent] -> PA.GlobalEnvironment SGEnvironment
sgEnvironmentFromAgents as = foldl (\accMap a -> (Map.insert (PA.agentId a) () accMap) ) Map.empty as

randomAgentState :: StdGen -> Double -> (SGAgentState, StdGen)
randomAgentState g p = (SIRSAgentState{ sgCurrState = s, sgPrevState = s, sgSumPayoff = 0.0, sgMaxPayoffState = s, sgMaxPayoffValue = 0.0 }, g')
    where
        (isDefector, g') = randomThresh g p
        (g'', _) = split g'
        s = if isDefector then
                Defector
                else
                    Cooperator

randomThresh :: StdGen -> Double -> (Bool, StdGen)
randomThresh g p = (flag, g')
    where
        (thresh, g') = randomR(0.0, 1.0) g
        flag = thresh <= p


agentNeighbours :: SGAgent -> [SGAgent] -> (Int, Int) -> [SGAgent]
agentNeighbours a as cells = filter (\a' -> any (==(agentToCell a' cells)) neighbourCells ) as
    where
        aCell = agentToCell a cells
        neighbourCells = neighbours aCell

agentToCell :: SGAgent -> (Int, Int) -> (Int, Int)
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
