module SpacialGame.SGModel where

import System.Random
import Data.Maybe
import Data.List

import qualified MinABS as ABS
import qualified Data.Map as Map

data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NeighbourPayoff (SGState, Double) | NeighbourState SGState deriving (Eq, Show)

data SGAgentState = SIRSAgentState {
    sgCoords :: (Int, Int),
    sgCurrState :: SGState,
    sgPrevState :: SGState,

    sgLocalPayoff :: Double,

    sgBestPayoff :: (SGState, Double),

    sgNeighbourFlag :: Int
} deriving (Show)

type SGAgent = ABS.Agent SGAgentState SGMsg
type SGTransformer = ABS.AgentTransformer SGAgentState SGMsg

bParam :: Double
bParam = 1.95

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0

sgTransformer :: SGTransformer
sgTransformer a ABS.Start = broadCastLocalState a
sgTransformer a (ABS.Dt dt) = a                         -- NOTE: no action on time-advance
sgTransformer a (ABS.Message (_, m)) = sgMsg a m        -- NOTE: ignore sender

sgMsg :: SGAgent -> SGMsg -> SGAgent
sgMsg a (NeighbourState s) = sgStateMsg a s
sgMsg a (NeighbourPayoff p) = sgPayoffMsg a p

sgStateMsg :: SGAgent -> SGState -> SGAgent
sgStateMsg a s = if ( allNeighboursTicked a'' ) then
                    broadCastLocalPayoff a''
                    else
                        a''
    where
        lp = sgLocalPayoff (ABS.s a)
        poIncrease = payoffWith a s
        newLp = lp + poIncrease
        a' = ABS.updateState a (\s -> s { sgLocalPayoff = newLp })
        a'' = tickNeighbourFlag a'

broadCastLocalPayoff :: SGAgent -> SGAgent
broadCastLocalPayoff a = resetNeighbourFlag a'
    where
        ls = sgCurrState (ABS.s a)
        lp = sgLocalPayoff (ABS.s a)
        a' = ABS.sendToNeighbours a (NeighbourPayoff (ls, lp))


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
            | v > localV = ABS.updateState a (\s -> s { sgBestPayoff = p } )
            | otherwise = a
            where
                (_, localV) = sgBestPayoff (ABS.s a)

switchToBestPayoff :: SGAgent -> SGAgent
switchToBestPayoff a = ABS.updateState a (\s -> s { sgCurrState = bestState,
                                                    sgPrevState = oldState,
                                                    sgLocalPayoff = 0.0,
                                                     sgBestPayoff = (bestState, 0.0)} )
    where
        (bestState, _) = sgBestPayoff (ABS.s a)
        oldState = sgCurrState (ABS.s a)

broadCastLocalState :: SGAgent -> SGAgent
broadCastLocalState a = resetNeighbourFlag a'
    where
        ls = sgCurrState (ABS.s a)
        a' = ABS.sendToNeighbours a (NeighbourState ls)

-- NOTE: the first state is always the owning agent
payoffWith :: SGAgent -> SGState -> Double
payoffWith a s = payoff as s
    where
        as = sgCurrState (ABS.s a)

payoff :: SGState -> SGState -> Double
payoff Defector Defector = pParam
payoff Cooperator Defector = sParam
payoff Defector Cooperator = bParam
payoff Cooperator Cooperator = rParam


allNeighboursTicked :: SGAgent -> Bool
allNeighboursTicked a = nf == 0
    where
        nf = (sgNeighbourFlag (ABS.s a))

tickNeighbourFlag :: SGAgent -> SGAgent
tickNeighbourFlag a = ABS.updateState a (\s -> s { sgNeighbourFlag = nf - 1 })
    where
        nf = (sgNeighbourFlag (ABS.s a))

resetNeighbourFlag :: SGAgent -> SGAgent
resetNeighbourFlag a = ABS.updateState a (\s -> s { sgNeighbourFlag = neighbourCount })
    where
        neighbourCount = length (ABS.ns a)

createRandomSGAgents :: (Int, Int) -> Double -> IO [SGAgent]
createRandomSGAgents cells@(x,y) p = do
                                        let ssIO = [ randomAgentState p (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        ss <- mapM id ssIO
                                        let as = map (\s -> ABS.createAgent (stateToAid s cells) s sgTransformer) ss
                                        let as' = map (addNeighbours as cells) as
                                        return as'
    where
        addNeighbours :: [SGAgent] -> (Int, Int) -> SGAgent -> SGAgent
        addNeighbours as cells a = resetNeighbourFlag a'
            where
                ns = agentNeighbours a as cells
                a' = foldl ABS.addNeighbour a ns

setDefector :: [SGAgent] -> (Int, Int) -> (Int, Int) -> [SGAgent]
setDefector as pos cells
    | isNothing mayAgentAtPos = as
    | otherwise = infront ++ [defectedAgentAtPos] ++ (tail behind)
    where
        mayAgentAtPos = find (\a -> pos == (agentToCell a cells)) as
        agentAtPos = (fromJust mayAgentAtPos)
        agentAtPosId = ABS.aid agentAtPos
        defectedAgentAtPos = ABS.updateState agentAtPos (\s -> s { sgCurrState = Defector,
                                                                    sgPrevState = Defector,
                                                                    sgBestPayoff = (Defector, 0.0) } )
        (infront, behind) = splitAt agentAtPosId as

stateToAid :: SGAgentState -> (Int, Int) -> ABS.Aid
stateToAid s (x, y) = (yCoord * x) + xCoord
    where
        (xCoord, yCoord) = sgCoords s

randomAgentState :: Double -> (Int, Int) -> IO SGAgentState
randomAgentState p coords = do
                                isDefector <- randomThresh p
                                let s = if isDefector then
                                            Defector
                                            else
                                                Cooperator
                                return SIRSAgentState{ sgCurrState = s,
                                        sgPrevState = s,
                                        sgLocalPayoff = 0.0,
                                        sgBestPayoff = (s, 0.0),
                                        sgNeighbourFlag = -1,
                                        sgCoords = coords }

randomThresh :: Double -> IO Bool
randomThresh p = do
                    thresh <- getStdRandom (randomR(0.0, 1.0))
                    return (thresh <= p)


agentNeighbours :: SGAgent -> [SGAgent] -> (Int, Int) -> [SGAgent]
agentNeighbours a as cells = filter (\a' -> any (==(agentToCell a' cells)) neighbourCells ) as
    where
        aCell = agentToCell a cells
        neighbourCells = neighbours aCell

agentToCell :: SGAgent -> (Int, Int) -> (Int, Int)
agentToCell a (xCells, yCells) = (ax, ay)
     where
        aid = ABS.aid a
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
