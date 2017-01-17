module SpacialGameEnv.SGModelEnv where

import System.Random

import qualified PureAgentsPar as PA
import qualified Data.Map as Map
import Data.Maybe

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NoMsg deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCurrState :: SGState,
    sgPrevState :: SGState
} deriving (Show)

type SGEnvironment = (Double, SGState)
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
sgTransformer ae (_, PA.Dt dt) = sgDt ae dt
sgTransformer (a, ge, le) (_, PA.Domain m) = (a, le)

-- TODO: is this really correct?
sgDt :: (SGAgent, PA.GlobalEnvironment SGEnvironment, PA.LocalEnvironment SGEnvironment) -> Double -> (SGAgent, PA.LocalEnvironment SGEnvironment)
sgDt (a, ge, _) dt = (a', le)
    where
        aid = PA.agentId a
        aSg = sgCurrState (PA.state a)
        neighbourIds = Map.keys (PA.neighbours a)
        localPayoff = foldl (\payoffSum nId -> payoffSum  + (payoffWithEnv aSg nId ge)) 0.0 neighbourIds
        (bestPayoff, bestState) = foldl (\best nId -> compareWithEnv best nId ge) (localPayoff, aSg) neighbourIds
        a' = PA.updateState a (\s -> s { sgCurrState = bestState, sgPrevState = aSg })
        le = (localPayoff, bestState)

payoffWithEnv :: SGState -> PA.AgentId -> PA.GlobalEnvironment SGEnvironment -> Double
payoffWithEnv aSg eId ge = payoff aSg eSg
    where
        (ePo, eSg) = fromJust (Map.lookup eId ge)

compareWithEnv :: (Double, SGState) -> PA.AgentId -> PA.GlobalEnvironment SGEnvironment -> (Double, SGState)
compareWithEnv bestPoSg@(bestPo, _) eId ge = if (ePo > bestPo) then
                                                envPoSg
                                                else
                                                    bestPoSg
    where
        envPoSg@(ePo, _)  = fromJust (Map.lookup eId ge)

payoff :: SGState -> SGState -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam


sgEnvironmentFromAgents :: [SGAgent] -> PA.GlobalEnvironment SGEnvironment
sgEnvironmentFromAgents as = foldl (\accMap a -> (Map.insert (PA.agentId a) (0.0, (sgCurrState (PA.state a))) accMap) ) Map.empty as

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
