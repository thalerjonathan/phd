module SpacialGame.SGModel where

import System.Random
import Data.Maybe
import Data.List

import qualified MinABS as ABS
import qualified Data.Map as Map

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
data SGState = Defector | Cooperator deriving (Eq, Show)
data SGMsg = NeighbourPayoff (SGState, Double) | NeighbourState SGState deriving (Eq, Show)

type SGCoord = (Int, Int)

data SGAgentState = SIRSAgentState {
    sgCoord :: SGCoord,
    sgCurrState :: SGState,
    sgPrevState :: SGState,

    sgLocalPayoff :: Double,

    sgBestPayoff :: (SGState, Double),

    sgNeighbourFlag :: Int,
    sgNeighbours :: [ABS.Aid]
} deriving (Show)

type SGAgent = ABS.Agent SGAgentState SGMsg
type SGTransformer = ABS.AgentTransformer SGAgentState SGMsg
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
bParam :: Double
bParam = 1.95

sParam :: Double
sParam = 0.0

pParam :: Double
pParam = 0.0

rParam :: Double
rParam = 1.0
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
sgTransformer :: SGTransformer
sgTransformer a ABS.Start = broadCastLocalState a
sgTransformer a (ABS.Dt dt) = a                         -- NOTE: no action on time-advance
sgTransformer a (ABS.Message (_, m)) = sgMsg a m        -- NOTE: ignore sender

sgMsg :: SGAgent -> SGMsg -> SGAgent
sgMsg a (NeighbourState s) = sgStateMsg a s
sgMsg a (NeighbourPayoff p) = sgPayoffMsg a p

-- TODO: need much more clarity here
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
        ns = sgNeighbours (ABS.s a)
        a' = ABS.broadcast a (ns, NeighbourPayoff (ls, lp))

-- TODO: need much more clarity here
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
        ns = sgNeighbours (ABS.s a)
        a' = ABS.broadcast a (ns, NeighbourState ls)

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
        ns = sgNeighbours (ABS.s a)
        neighbourCount = length ns
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
createRandomSGAgents :: (Int, Int) -> Double -> IO [SGAgent]
createRandomSGAgents max@(x,y) p = do
                                        let ssIO = [ randomAgentState p max (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        ss <- mapM id ssIO
                                        let as = map (\s -> ABS.createAgent (stateToAid s max) s sgTransformer) ss
                                        return as
    where
        stateToAid :: SGAgentState -> (Int, Int) -> ABS.Aid
        stateToAid s max = coordToAid max c
            where
                c = sgCoord s

randomAgentState :: Double -> (Int, Int)  -> SGCoord -> IO SGAgentState
randomAgentState p max coord = do
                                    r <- getStdRandom (randomR(0.0, 1.0))
                                    let isDefector = r <= p

                                    let s = if isDefector then
                                                Defector
                                                else
                                                    Cooperator

                                    let nCoords = neighbours coord max
                                    let nIds = map (coordToAid max) nCoords

                                    return SIRSAgentState{
                                            sgCoord = coord,
                                            sgCurrState = s,
                                            sgPrevState = s,
                                            sgLocalPayoff = 0.0,
                                            sgBestPayoff = (s, 0.0),
                                            sgNeighbourFlag = 0,
                                            sgNeighbours = nIds }

coordToAid :: (Int, Int) -> SGCoord -> ABS.Aid
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x

neighbours :: SGCoord -> (Int, Int) -> [SGCoord]
neighbours (x,y) max = clipCoords allCoords max
    where
        allCoords = map (\(x', y') -> (x+x', y+y')) neighbourhood

clipCoords :: [SGCoord] -> (Int, Int) -> [SGCoord]
clipCoords cs max = filter (\c -> validCoord c max ) cs
    where
        validCoord :: SGCoord -> (Int, Int) -> Bool
        validCoord (x, y) (xMax, yMax)
            | x < 0 = False
            | y < 0 = False
            | x >= xMax = False
            | y >= yMax = False
            | otherwise = True

neighbourhood :: [SGCoord]
neighbourhood = [topLeft, top, topRight,
                 left, center, right,
                 bottomLeft, bottom, bottomRight]
    where
        topLeft =       (-1, -1)
        top =           ( 0, -1)
        topRight =      ( 1, -1)
        left =          (-1,  0)
        center =        ( 0,  0)
        right =         ( 1,  0)
        bottomLeft =    (-1,  1)
        bottom =        ( 0,  1)
        bottomRight =   ( 1,  1)

setDefector :: [SGAgent] -> SGCoord -> (Int, Int) -> [SGAgent]
setDefector as coord cells
    | isNothing mayAgentAtPos = as
    | otherwise = infront ++ [defectedAgentAtPos] ++ (tail behind)
    where
        mayAgentAtPos = find (\a -> coord == (sgCoord (ABS.s a))) as
        agentAtPos = (fromJust mayAgentAtPos)
        agentAtPosId = ABS.aid agentAtPos
        defectedAgentAtPos = ABS.updateState agentAtPos (\s -> s { sgCurrState = Defector,
                                                                    sgPrevState = Defector,
                                                                    sgBestPayoff = (Defector, 0.0) } )
        (infront, behind) = splitAt agentAtPosId as
------------------------------------------------------------------------------------------------------------------------