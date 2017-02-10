module SIRS.SIRSModel where

import System.Random

import qualified MinABS as ABS
import qualified Data.Map as Map

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState

type SIRSCoord = (Int, Int)

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsCoord :: SIRSCoord,
    sirsTime :: Double,
    sirsNeighbours :: [ABS.Aid],
    sirsRng :: StdGen
} deriving (Show)

type SIRSAgent = ABS.Agent SIRSAgentState SIRSMsg
type SIRSTransformer = ABS.AgentTransformer SIRSAgentState SIRSMsg
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3.0

infectionProbability :: Double
infectionProbability = 0.3
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
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
    | yes = ABS.updateState a (\s -> s { sirsState = Infected,
                                            sirsTime = 0.0} )
    | otherwise = a'
    where
        (a', yes) = drawInfectionWithProb a infectionProbability

drawInfectionWithProb :: SIRSAgent -> Double -> (SIRSAgent, Bool)
drawInfectionWithProb a p = (a', infect)
    where
        (a', r) = drawRandom a
        infect = r <= p

drawRandom :: SIRSAgent -> (SIRSAgent, Double)
drawRandom a = (a', r)
    where
        g = (sirsRng (ABS.s a))
        (r, g') = randomR (0.0, 1.0) g
        a' = ABS.updateState a (\s -> s { sirsRng = g' } )

-- TODO: need more clarity here
handleInfectedAgent :: SIRSAgent -> Double -> SIRSAgent
handleInfectedAgent a dt = if t' >= infectedDuration then
                                recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
                                else
                                    randomContact gettingBetterAgent

    where
        t = (sirsTime (ABS.s a))
        t' = t + dt
        recoveredAgent = ABS.updateState a (\s -> s { sirsState = Recovered,
                                                        sirsTime = 0.0 } )
        gettingBetterAgent = ABS.updateState a (\s -> s { sirsTime = t' } )


-- TODO: need more clarity here
handleRecoveredAgent :: SIRSAgent -> Double -> SIRSAgent
handleRecoveredAgent a dt = if t' >= immuneDuration then
                                susceptibleAgent
                                else
                                    immuneReducedAgent
    where
        t = (sirsTime (ABS.s a))
        t' = t + dt
        susceptibleAgent = ABS.updateState a (\s -> s { sirsState = Susceptible,
                                                        sirsTime = 0.0 } )
        immuneReducedAgent = ABS.updateState a (\s -> s { sirsTime = t' } )


-- TODO: need more clarity here
randomContact :: SIRSAgent -> SIRSAgent
randomContact a = ABS.send a' (randNeigh, (Contact Infected))
    where
        nsCount = length (sirsNeighbours (ABS.s a))
        g = (sirsRng (ABS.s a))
        (randIdx, g') = randomR(0, nsCount-1) g
        randNeigh = (sirsNeighbours (ABS.s a)) !! randIdx
        a' = ABS.updateState a (\s -> s { sirsRng = g' } )


is :: SIRSAgent -> SIRSState -> Bool
is a ss = (sirsState s) == ss
    where
        s = ABS.s a

------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
createRandomSIRSAgents :: (Int, Int) -> Double -> IO [SIRSAgent]
createRandomSIRSAgents max@(x,y) p =  do
                                           let ssIO = [ randomAgentState p max (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                           ss <- mapM id ssIO
                                           let as = map (\s -> ABS.createAgent (stateToAid s max) s sirsTransformer) ss
                                           return as
    where
       stateToAid :: SIRSAgentState -> (Int, Int) -> ABS.Aid
       stateToAid s max = coordToAid max c
           where
               c = sirsCoord s

randomAgentState :: Double -> (Int, Int) -> SIRSCoord -> IO SIRSAgentState
randomAgentState p max coord = do
                                    r <- getStdRandom (randomR(0.0, 1.0))
                                    let isInfected = r <= p

                                    let s = if isInfected then
                                                Infected
                                                else
                                                    Susceptible

                                    let nCoords = neighbours coord max
                                    let nIds = map (coordToAid max) nCoords
                                    rng <- newStdGen

                                    return SIRSAgentState{
                                            sirsState = s,
                                            sirsCoord = coord,
                                            sirsTime = 0.0,
                                            sirsNeighbours = nIds,
                                            sirsRng = rng }


coordToAid :: (Int, Int) -> SIRSCoord -> ABS.Aid
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x

neighbours :: SIRSCoord -> (Int, Int) -> [SIRSCoord]
neighbours (x,y) max = clipCoords allCoords max
    where
        allCoords = map (\(x', y') -> (x+x', y+y')) neighbourhood

clipCoords :: [SIRSCoord] -> (Int, Int) -> [SIRSCoord]
clipCoords cs max = filter (\c -> validCoord c max ) cs
    where
        validCoord :: SIRSCoord -> (Int, Int) -> Bool
        validCoord (x, y) (xMax, yMax)
            | x < 0 = False
            | y < 0 = False
            | x >= xMax = False
            | y >= yMax = False
            | otherwise = True

neighbourhood :: [(Int, Int)]
neighbourhood = [topLeft, top, topRight,
                 left, right,
                 bottomLeft, bottom, bottomRight]
    where
        topLeft =       (-1, -1)
        top =           ( 0, -1)
        topRight =      ( 1, -1)
        left =          (-1,  0)
        right =         ( 1,  0)
        bottomLeft =    (-1,  1)
        bottom =        ( 0,  1)
        bottomRight =   ( 1,  1)
------------------------------------------------------------------------------------------------------------------------