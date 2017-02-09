module SpacialGameMsg.SGModelMsg where

import qualified PureAgentsSeq as PA

import qualified Data.Map as Map

import System.Random

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NeighbourPayoff (SGState, Double) | NeighbourState SGState deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCurrState :: SGState,
    sgPrevState :: SGState,

    sgLocalPayoff :: Double,

    sgBestPayoff :: (SGState, Double),

    sgNeighbourFlag :: Int
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
sgTransformer (a, e) PA.Start = (broadCastLocalState a, e)
sgTransformer ae (PA.Dt dt) = ae                                    -- NOTE: no action on time-advance
sgTransformer (a, e) (PA.Message (_, m)) = (sgMsg a m, e)           -- NOTE: ignore sender

sgMsg :: SGAgent -> SGMsg -> SGAgent
sgMsg a (NeighbourState s) = sgStateMsg a s
sgMsg a (NeighbourPayoff p) = sgPayoffMsg a p

sgStateMsg :: SGAgent -> SGState -> SGAgent
sgStateMsg a s = if ( allNeighboursTicked a'' ) then
                    broadCastLocalPayoff a''
                    else
                        a''
    where
        lp = sgLocalPayoff (PA.state a)
        poIncrease = payoffWith a s
        newLp = lp + poIncrease
        a' = PA.updateState a (\s -> s { sgLocalPayoff = newLp })
        a'' = tickNeighbourFlag a'

broadCastLocalPayoff :: SGAgent -> SGAgent
broadCastLocalPayoff a = resetNeighbourFlag a'
    where
        ls = sgCurrState (PA.state a)
        lp = sgLocalPayoff (PA.state a)
        a' = PA.broadcastMsgToNeighbours a (NeighbourPayoff (ls, lp))


sgPayoffMsg :: SGAgent -> (SGState, Double) -> SGAgent
sgPayoffMsg a p = if ( allNeighboursTicked a'' ) then
                      broadCastLocalState $ switchToBestPayoff a''
                      else
                          a''
    where
        a' = comparePayoff a p
        a'' = tickNeighbourFlag a'

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

broadCastLocalState :: SGAgent -> SGAgent
broadCastLocalState a = resetNeighbourFlag a'
    where
        ls = sgCurrState (PA.state a)
        a' = PA.broadcastMsgToNeighbours a (NeighbourState ls)

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


allNeighboursTicked :: SGAgent -> Bool
allNeighboursTicked a = nf == 0
    where
        nf = (sgNeighbourFlag (PA.state a))

tickNeighbourFlag :: SGAgent -> SGAgent
tickNeighbourFlag a = PA.updateState a (\s -> s { sgNeighbourFlag = nf - 1 })
    where
        nf = (sgNeighbourFlag (PA.state a))

resetNeighbourFlag :: SGAgent -> SGAgent
resetNeighbourFlag a = PA.updateState a (\s -> s { sgNeighbourFlag = neighbourCount })
    where
        neighbourCount = Map.size (PA.neighbours a)


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
randomAgentState g p = (SIRSAgentState{ sgCurrState = s,
                                        sgPrevState = s,
                                        sgLocalPayoff = 0.0,
                                        sgBestPayoff = (s, 0.0),
                                        sgNeighbourFlag = 0}, g')
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
