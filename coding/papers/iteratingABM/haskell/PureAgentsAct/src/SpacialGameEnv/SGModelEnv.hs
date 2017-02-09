module SpacialGameEnv.SGModelEnv where

import System.Random
import Data.Maybe
import qualified Data.Map as Map
import Control.Concurrent.STM.TVar
import Control.Monad.STM

import qualified PureAgentsAct as PA

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NoMsg deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCurrState :: SGState,
    sgPrevState :: SGState
} deriving (Show)

type SGEnvironment = Map.Map PA.AgentId (TVar (Double, SGState))
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
sgTransformer (a, e) PA.Start = return a
sgTransformer ae (PA.Dt (t, dt)) = sgDt ae dt
sgTransformer (a, _) (PA.Message m) = return a

payoffWithEnv :: SGState -> PA.AgentId -> SGEnvironment -> STM Double
payoffWithEnv aSg eId e = do
                            (ePo, eSg) <- readTVar eVar
                            let po = payoff aSg eSg
                            return po
    where
        eVar = fromJust (Map.lookup eId e)

compareWithEnv :: STM (Double, SGState) -> PA.AgentId -> SGEnvironment -> STM (Double, SGState)
compareWithEnv bestPoSgSTM eId e = do
                                    bestPoSg@(bestPo, _) <- bestPoSgSTM
                                    envPoSg@(ePo, _) <- readTVar eVar
                                    if (ePo > bestPo) then
                                        return envPoSg
                                        else
                                            return bestPoSg
    where
        eVar = fromJust (Map.lookup eId e)

payoff :: SGState -> SGState -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam

stmAdd :: STM Double -> STM Double -> STM Double
stmAdd xM yM = do
                x <- xM
                y <- yM
                return (x+y)

sgDt :: (SGAgent, SGEnvironment) -> Double -> STM SGAgent
sgDt (a, e) dt = do
                    localPayoff <- foldl (\payoffSum nId -> stmAdd payoffSum (payoffWithEnv aSg nId e)) (return 0.0) neighbourIds
                    (bestPayoff, bestState) <- foldl (\best nId -> compareWithEnv best nId e) (return (localPayoff, aSg)) neighbourIds
                    let a' = PA.updateState a (\s -> s { sgCurrState = bestState, sgPrevState = aSg })
                    let eVar = fromJust (Map.lookup aid e)
                    writeTVar eVar (localPayoff, bestState)
                    return a'
    where
        aid = PA.agentId a
        aSg = sgCurrState (PA.state a)
        neighbourIds = Map.keys (PA.neighbours a)

sgEnvironmentFromAgents :: [SGAgent] -> STM SGEnvironment
sgEnvironmentFromAgents as = foldl insertCell (return Map.empty) as
    where
        insertCell :: STM (Map.Map PA.AgentId (TVar (Double, SGState))) -> SGAgent -> STM (Map.Map PA.AgentId (TVar (Double, SGState)))
        insertCell m a = do
                            let aid = PA.agentId a
                            let tup = (0.0, (sgCurrState (PA.state a)))
                            cVar <- newTVar tup
                            m' <- m
                            return (Map.insert aid cVar m')

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
randomAgentState g p = (SIRSAgentState{ sgCurrState = s, sgPrevState = s }, g')
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
