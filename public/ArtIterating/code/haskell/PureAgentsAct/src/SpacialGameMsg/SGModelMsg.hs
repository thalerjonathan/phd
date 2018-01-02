module SpacialGameMsg.SGModelMsg where

import System.Random
import Control.Monad.STM
import Debug.Trace

import qualified PureAgentsAct as PA
import qualified Data.Map as Map

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NeighbourPayoff (SGState, Double) | NeighbourState SGState deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCurrState :: SGState,
    sgPrevState :: SGState,

    sgLocalPayoff :: Double,

    sgBestPayoff :: (SGState, Double),

    sgNeighbourPayoffCount :: Int,
    sgNeighbourStateCount :: Int
} deriving (Show)

type SGEnvironment = ()
type SGAgent = PA.Agent SGMsg SGAgentState SGEnvironment
type SGTransformer = PA.AgentTransformer SGMsg SGAgentState SGEnvironment
type SGSimHandle = PA.SimHandle SGMsg SGAgentState SGEnvironment

bParam :: Double
bParam = 1.95

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0

sgTransformer :: SGTransformer
sgTransformer (a, _) PA.Start = sgStart a
sgTransformer (a, e) (PA.Dt dt) = return a
sgTransformer (a, e) (PA.Message (_,m)) = sgMsg a m

sgStart :: SGAgent -> STM SGAgent
sgStart a = (broadCastLocalState a'')
    where
        a' = resetNeighbourPayoffCount a
        a'' = resetNeighbourStateCount a'

sgMsg :: SGAgent -> SGMsg -> STM SGAgent
sgMsg a (NeighbourState s) = sgStateMsg a s
sgMsg a (NeighbourPayoff p) = sgPayoffMsg a p

sgStateMsg :: SGAgent -> SGState -> STM SGAgent
sgStateMsg a s = do
                    if ( allNeighboursStateCountTicked a' ) then
                        broadCastLocalPayoff $ resetNeighbourStateCount a'
                        else
                            return a'
    where
        a' = tickNeighbourStateCount $ playGame a s

        playGame :: SGAgent -> SGState -> SGAgent
        playGame a s  = a'
            where
                lp = sgLocalPayoff (PA.state a)
                poIncrease = payoffWith a s
                newLp = lp + poIncrease
                a' = PA.updateState a (\s -> s { sgLocalPayoff = newLp })

broadCastLocalPayoff :: SGAgent -> STM SGAgent
broadCastLocalPayoff a = do
                            PA.broadcastMsgToNeighbours a (NeighbourPayoff (ls, lp))
                            return a
    where
        ls = sgCurrState (PA.state a)
        lp = sgLocalPayoff (PA.state a)

sgPayoffMsg :: SGAgent -> (SGState, Double) -> STM SGAgent
sgPayoffMsg a p = if ( allNeighboursPayoffCountTicked a' ) then
                      broadCastLocalState $ resetNeighbourPayoffCount $ switchToBestPayoff a'
                      else
                          return a'
    where
        a' = tickNeighbourPayoffCount $ comparePayoff a p

        comparePayoff :: SGAgent -> (SGState, Double) -> SGAgent
        comparePayoff a p@(_, v)
            | v > localV = PA.updateState a (\s -> s { sgBestPayoff = p } )
            | otherwise = a
            where
                (_, localV) = sgBestPayoff (PA.state a)

switchToBestPayoff :: SGAgent -> SGAgent
switchToBestPayoff a = PA.updateState a (\s -> s { sgCurrState = bestState,
                                                    sgPrevState = oldState,
                                                    sgLocalPayoff = 0.0,
                                                     sgBestPayoff = (bestState, 0.0)} )
    where
        (bestState, _) = sgBestPayoff (PA.state a)
        oldState = sgCurrState (PA.state a)

broadCastLocalState :: SGAgent -> STM SGAgent
broadCastLocalState a = do
                            PA.broadcastMsgToNeighbours a (NeighbourState ls)
                            return a
    where
        ls = sgCurrState (PA.state a)

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


allNeighboursPayoffCountTicked :: SGAgent -> Bool
allNeighboursPayoffCountTicked a = nf == 0
    where
        nf = (sgNeighbourPayoffCount (PA.state a))

allNeighboursStateCountTicked :: SGAgent -> Bool
allNeighboursStateCountTicked a = nf == 0
    where
        nf = (sgNeighbourStateCount (PA.state a))

tickNeighbourPayoffCount :: SGAgent -> SGAgent
tickNeighbourPayoffCount a = PA.updateState a (\s -> s { sgNeighbourPayoffCount = nf - 1 })
    where
        nf = (sgNeighbourPayoffCount (PA.state a))

tickNeighbourStateCount :: SGAgent -> SGAgent
tickNeighbourStateCount a = PA.updateState a (\s -> s { sgNeighbourStateCount = nf - 1 })
    where
        nf = (sgNeighbourStateCount (PA.state a))

resetNeighbourPayoffCount :: SGAgent -> SGAgent
resetNeighbourPayoffCount a = PA.updateState a (\s -> s { sgNeighbourPayoffCount = neighbourCount })
    where
        neighbourCount = Map.size (PA.neighbours a)

resetNeighbourStateCount :: SGAgent -> SGAgent
resetNeighbourStateCount a = PA.updateState a (\s -> s { sgNeighbourStateCount = neighbourCount })
    where
        neighbourCount = Map.size (PA.neighbours a)


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
randomAgentState g p = (SIRSAgentState{ sgCurrState = s,
                                                               sgPrevState = s,
                                                               sgLocalPayoff = 0.0,
                                                               sgBestPayoff = (s, 0.0),
                                                               sgNeighbourPayoffCount = 0,
                                                               sgNeighbourStateCount = 0}, g')
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
