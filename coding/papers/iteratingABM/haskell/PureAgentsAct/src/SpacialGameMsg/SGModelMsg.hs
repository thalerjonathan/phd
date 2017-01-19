module SpacialGameMsg.SGModelMsg where

import System.Random
import Control.Monad.STM

import qualified PureAgentsAct as PA

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
sgTransformer (a, e) (_, PA.Dt (t, dt)) = sgDt a dt
sgTransformer (a, e) (_, PA.Domain m) = sgMsg a m

sgMsg :: SGAgent -> SGMsg -> STM SGAgent
sgMsg a (NeighbourAction s) = sgActionMsg a s
sgMsg a (NeighbourPayoff s p) = sgPayoffMsg a s p

sgActionMsg :: SGAgent -> SGState -> STM SGAgent
sgActionMsg a s = return (PA.updateState a (\s -> s { sgSumPayoff = newPo }))
    where
        po = sgSumPayoff (PA.state a)
        poIncrease = payoffWith a s
        newPo = po + poIncrease

sgPayoffMsg :: SGAgent -> SGState -> Double -> STM SGAgent
sgPayoffMsg a sg p
    | p > poMaxVal = return (PA.updateState a (\s -> s { sgMaxPayoffValue = p, sgMaxPayoffState = sg } ))
    | otherwise = return a
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
sgDt :: SGAgent -> Double -> STM SGAgent
sgDt a dt = do
                PA.broadcastMsgToNeighbours a' (NeighbourPayoff localState localPayoff)
                PA.broadcastMsgToNeighbours a' (NeighbourAction bestState)
                return a'
    where
        localState = sgCurrState (PA.state a)
        localPayoff = sgSumPayoff (PA.state a)

        bestPayoff = sgMaxPayoffValue (PA.state a)
        bestState = sgMaxPayoffState (PA.state a)

        a' = PA.updateState a (\s -> s { sgCurrState = bestState, sgPrevState = localState } )


createRandomSGAgents :: StdGen -> (Int, Int) -> Double -> STM ([SGAgent], StdGen)
createRandomSGAgents gInit cells@(x,y) p = do
                                            as <- mapM (\idx -> PA.createAgent idx (randStates !! idx) sgTransformer) [0..n-1]
                                            let as' = map (\a -> PA.addNeighbours a (agentNeighbours a as cells) ) as
                                            return (as', g')
    where
        n = x * y
        (randStates, g') = createRandomStates gInit n p

        createRandomStates :: StdGen -> Int -> Double -> ([SGAgentState], StdGen)
        createRandomStates g 0 p = ([], g)
        createRandomStates g n p = (rands, g'')
            where
              (randState, g') = randomAgentState g p
              (ras, g'') = createRandomStates g' (n-1) p
              rands = randState : ras

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
