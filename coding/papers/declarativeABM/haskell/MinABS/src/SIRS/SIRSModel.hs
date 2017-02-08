module SIRS.SIRSModel where

import System.Random

import qualified MinABS as ABS
import qualified Data.Map as Map

data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState

data SIRSAgentState = SIRSAgentState {
    sirState :: SIRSState,
    timeInState :: Double,
    rng :: StdGen
} deriving (Show)

type SIRSAgent = ABS.Agent SIRSAgentState SIRSMsg
type SIRSTransformer = ABS.AgentTransformer SIRSAgentState SIRSMsg

infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3.0

infectionProbability :: Double
infectionProbability = 0.3

is :: SIRSAgent -> SIRSState -> Bool
is a ss = (sirState s) == ss
    where
        s = ABS.s a

sirsTransformer :: SIRSTransformer
sirsTransformer a ABS.Start = a
sirsTransformer a (ABS.Dt dt) = sirsDt a dt
sirsTransformer a (ABS.Message (_, m)) = sirsMsg a m       -- NOTE: ignore sender

sirsMsg :: SIRSAgent -> SIRSMsg -> SIRSAgent
sirsMsg a (Contact Infected)
    | is a Susceptible = infectAgent a
    | otherwise = a
sirsMsg a (Contact _) = a

sirsDt :: SIRSAgent -> Double -> SIRSAgent
sirsDt a dt
    | is a Susceptible = a
    | is a Infected = handleInfectedAgent a dt
    | is a Recovered = handleRecoveredAgent a dt

infectAgent :: SIRSAgent -> SIRSAgent
infectAgent a
    | infect = ABS.updateState a (\s -> s { sirState = Infected, timeInState = 0.0, rng = g' } )
    | otherwise = ABS.updateState a (\s -> s { rng = g' } )
    where
        g = (rng (ABS.s a))
        (infect, g') = randomThresh g infectionProbability

handleInfectedAgent :: SIRSAgent -> Double -> SIRSAgent
handleInfectedAgent a dt = if t' >= infectedDuration then
                                recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
                                else
                                    randomContact gettingBetterAgent

    where
        t = (timeInState (ABS.s a))
        t' = t + dt
        recoveredAgent = ABS.updateState a (\s -> s { sirState = Recovered, timeInState = 0.0 } )
        gettingBetterAgent = ABS.updateState a (\s -> s { timeInState = t' } )

handleRecoveredAgent :: SIRSAgent -> Double -> SIRSAgent
handleRecoveredAgent a dt = if t' >= immuneDuration then
                                susceptibleAgent
                                else
                                    immuneReducedAgent
    where
        t = (timeInState (ABS.s a))
        t' = t + dt
        susceptibleAgent = ABS.updateState a (\s -> s { sirState = Susceptible, timeInState = 0.0 } )
        immuneReducedAgent = ABS.updateState a (\s -> s { timeInState = t' } )


randomContact :: SIRSAgent -> SIRSAgent
randomContact a = ABS.send a' (randNeigh, (Contact Infected))
    where
        nsCount = length (ABS.ns a)
        g = (rng (ABS.s a))
        (randIdx, g') = randomR(0, nsCount-1) g
        randNeigh = (ABS.ns a) !! randIdx
        a' = ABS.updateState a (\s -> s { rng = g' } )

createRandomSIRSAgents :: StdGen -> (Int, Int) -> Double -> ([SIRSAgent], StdGen)
createRandomSIRSAgents gInit cells@(x,y) p = (as', g')
    where
        n = x * y
        (randStates, g') = createRandomStates gInit n p
        as = map (\idx -> ABS.createAgent idx (randStates !! idx) sirsTransformer) [0..n-1]
        --as' = map (\a -> PA.addNeighbours a (filter (\a' -> (PA.agentId a') /= (PA.agentId a)) as) ) as
        as' = map (addNeighbours as) as

        addNeighbours :: [SIRSAgent] -> SIRSAgent -> SIRSAgent
        addNeighbours as a = foldl ABS.addNeighbour a ns
            where
                ns = (agentNeighbours a as cells)

        createRandomStates :: StdGen -> Int -> Double -> ([SIRSAgentState], StdGen)
        createRandomStates g 0 p = ([], g)
        createRandomStates g n p = (rands, g'')
            where
              (randState, g') = randomAgentState g p
              (ras, g'') = createRandomStates g' (n-1) p
              rands = randState : ras

randomAgentState :: StdGen -> Double -> (SIRSAgentState, StdGen)
randomAgentState g p = (SIRSAgentState{ sirState = s,
                                        timeInState = 0.0,
                                        rng = g'' }, g')
    where
        (isInfected, g') = randomThresh g p
        (g'', _) = split g'
        s = if isInfected then
                Infected
                else
                    Susceptible

randomThresh :: StdGen -> Double -> (Bool, StdGen)
randomThresh g p = (flag, g')
    where
        (thresh, g') = randomR(0.0, 1.0) g
        flag = thresh <= p

agentNeighbours :: SIRSAgent -> [SIRSAgent] -> (Int, Int) -> [SIRSAgent]
agentNeighbours a as cells = filter (\a' -> any (==(agentToCell a' cells)) neighbourCells ) as
    where
        aCell = agentToCell a cells
        neighbourCells = neighbours aCell

agentToCell :: SIRSAgent -> (Int, Int) -> (Int, Int)
agentToCell a (xCells, yCells) = (ax, ay)
     where
        aid = ABS.aid a
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
