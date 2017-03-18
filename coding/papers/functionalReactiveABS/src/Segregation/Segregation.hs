{-# LANGUAGE Arrows #-}
module Segregation.Segregation where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import System.IO
import System.Random
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SegAgentType = Red | Green deriving (Eq, Show)
type SegMsg = ()    -- Agents are not communicating in Schelling Segregation

data SegMoveStrategy = Local | Global deriving (Eq)
data SegOptStrategy = None | OptimizePresent Int | OptimizieRecursive Int deriving (Eq) -- TODO: add recursion depth (1)

type SegCoord = (Int, Int)

data SegAgentState = SegAgentState {
    segAgentType :: SegAgentType,
    segCoord :: SegCoord,
    segSimilarityWanted :: Double,
    segSimilarityCurrent :: Double,
    segRng :: StdGen
} deriving (Show)

type SegEnvCell = Maybe SegAgentType
type SegEnvironment = Environment SegEnvCell

type SegAgentDef = AgentDef SegAgentState SegMsg SegEnvCell
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg SegEnvCell
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvCell
------------------------------------------------------------------------------------------------------------------------

-- TODO implement recursive simulations: MetaABS: test it using the segregation model. run recursive simulation for local neighbourhood only
    -- for each move compute 1 step recursive and see how this move performs. if it improves the outcome, then take it otherwise stay.

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
similarWanted :: Double
similarWanted = 0.9

density :: Double
density = 0.8

redGreenDist :: Double
redGreenDist = 0.5

freeCellRetries :: Int
freeCellRetries = 3

localMovementRadius :: Int
localMovementRadius = 2

movementStrategy :: SegMoveStrategy
movementStrategy = Local

optimizingStrategy :: SegOptStrategy
optimizingStrategy = OptimizePresent 5
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
is :: SegAgentOut -> SegAgentType -> Bool
is ao sat = (segAgentType s) == sat
    where
        s = aoState ao

isOccupied :: SegEnvCell -> Bool
isOccupied = isJust

segDt :: SegAgentOut -> Double -> SegAgentOut
segDt ao dt
    | isHappy ao = ao
    | otherwise = move ao

move :: SegAgentOut -> SegAgentOut
move ao
    | optCoordFound = moveTo ao' optCoord
    | otherwise = ao'
    where
        (ao', mayOptCoord) = findOptMove optimizingStrategy ao
        optCoordFound = isJust mayOptCoord
        optCoord = fromJust mayOptCoord

findOptMove :: SegOptStrategy -> SegAgentOut -> (SegAgentOut, Maybe EnvCoord)
findOptMove None ao = findFreeCoord ao freeCellRetries
findOptMove (OptimizePresent retries) ao = findOptMoveAux ao retries
    where
        findOptMoveAux :: SegAgentOut -> Int -> (SegAgentOut, Maybe EnvCoord)
        findOptMoveAux ao 0 = findFreeCoord ao freeCellRetries
        findOptMoveAux ao retries
            | freeCoordFound = if moveImproves ao' freeCoord then ret else findOptMoveAux ao' (retries - 1)
            | otherwise = ret
            where
                ret@(ao', mayFreeCoord) = findFreeCoord ao freeCellRetries
                freeCoordFound = isJust mayFreeCoord
                freeCoord = fromJust mayFreeCoord

        moveImproves :: SegAgentOut -> EnvCoord -> Bool
        moveImproves ao coord = similiarityOnCoord > similiarityCurrent
            where
                similiarityOnCoord = calculateSimilarity ao coord
                similiarityCurrent = (segSimilarityCurrent (aoState ao))

moveTo :: SegAgentOut -> EnvCoord -> SegAgentOut
moveTo ao newCoord = ao''
    where
        s = aoState ao
        env = aoEnv ao
        oldCoord = segCoord s
        c = segAgentType s
        env' = changeCellAt env oldCoord Nothing
        env'' = changeCellAt env' newCoord (Just c)
        ao' = ao { aoEnv = env'' }
        newSimilarity = calculateSimilarity ao' newCoord
        ao'' = updateState ao' (\s -> s { segCoord = newCoord, segSimilarityCurrent = newSimilarity } )

findFreeCoord :: SegAgentOut -> Int -> (SegAgentOut, Maybe EnvCoord)
findFreeCoord ao 0 = (ao, Nothing)
findFreeCoord ao maxRetries
    | isOccupied randCell = findFreeCoord ao' (maxRetries - 1)
    | otherwise = (ao', Just randCoord)
    where
        (ao', randCell, randCoord) = findFreeCoordAux movementStrategy ao

        findFreeCoordAux :: SegMoveStrategy -> SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
        findFreeCoordAux strat ao
            | Local == strat = localRandomCell ao
            | Global == strat = globalRandomCell ao

localRandomCell :: SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
localRandomCell ao = (ao', randCell, randCoord)
    where
        s = aoState ao
        env = aoEnv ao
        g = segRng s
        originCoord = segCoord s
        (randCell, randCoord, g') = randomCellWithRadius g env originCoord localMovementRadius
        ao' = updateState ao (\s -> s { segRng = g' } )

globalRandomCell :: SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
globalRandomCell ao = (ao', randCell, randCoord)
    where
        s = aoState ao
        env = aoEnv ao
        g = segRng s
        (randCell, randCoord, g') = randomCell g env
        ao' = updateState ao (\s -> s { segRng = g' } )

-- TODO: need to calculate 'as if' the agent was on the given coord
calculateSimilarity :: SegAgentOut -> SegCoord -> Double
calculateSimilarity ao coord
    | is ao Red = (fromInteger $ fromIntegral reds) / totalCount
    | is ao Green = (fromInteger $ fromIntegral greens) / totalCount
    where
        s = aoState ao
        env = aoEnv ao
        cs = neighbours env coord
        (reds, greens) = countOccupied cs
        totalCount = (fromInteger $ fromIntegral (reds + greens))

        countOccupied :: [SegEnvCell] -> (Int, Int)
        countOccupied cs = (redCount, greenCount)
            where
                occupiedCells = filter isOccupied cs
                redCount = length $ filter ((==Red) . fromJust) occupiedCells
                greenCount = length $ filter ((==Green) . fromJust) occupiedCells

makesHappy :: SegAgentOut -> SegCoord -> Bool
makesHappy ao coord = similarityOnCoord >= similarityWanted
    where
        s = aoState ao
        similarityOnCoord = calculateSimilarity ao coord
        similarityWanted = segSimilarityWanted s

isHappy :: SegAgentOut -> Bool
isHappy ao = makesHappy ao (segCoord (aoState ao))


------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
------------------------------------------------------------------------------------------------------------------------
createSegAgentsAndEnv :: (Int, Int) -> IO ([SegAgentDef], SegEnvironment)
createSegAgentsAndEnv limits@(x,y) =  do
                                        let coords = [ (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        (asDefs, envCells) <- populateEnv limits coords
                                        let env = createEnvironment
                                                              Nothing
                                                              limits
                                                              moore
                                                              ClipToMax
                                                              envCells

                                        let as = map (\s -> createAgent s limits) asDefs
                                        return (as, env)
    where
        populateEnv :: (Int, Int) -> [(Int, Int)] -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
        populateEnv max coords = foldr (populateEnvAux max) (return ([], [])) coords

            where
                populateEnvAux :: (Int, Int)
                                    -> (Int, Int)
                                    -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
                                    -> IO ([SegAgentState], [(EnvCoord, SegEnvCell)])
                populateEnvAux max coord accIO = do
                                                    (accAs, accCells) <- accIO

                                                    ra <- randomAgentState max coord

                                                    let emptyCell = (coord, Nothing)
                                                    let occupiedCell = (coord, Just (segAgentType ra))

                                                    r <- getStdRandom (randomR(0.0, 1.0))
                                                    if r < density then
                                                        return ((ra : accAs), occupiedCell : accCells)
                                                        else
                                                            return (accAs, emptyCell : accCells)

        createAgent :: SegAgentState -> (Int, Int) -> SegAgentDef
        createAgent s max = AgentDef { adId = agentId,
                                        adState = s,
                                        adBehaviour = segAgentBehaviour }
            where
                c = segCoord s
                agentId = coordToAid max c

randomAgentState :: (Int, Int) -> SegCoord -> IO SegAgentState
randomAgentState max coord = do
                                r <- getStdRandom (randomR(0.0, 1.0))
                                let isRed = (r <= redGreenDist)

                                let s = if isRed then
                                            Red
                                            else
                                                Green

                                rng <- newStdGen

                                return SegAgentState {
                                        segAgentType = s,
                                        segCoord = coord,
                                        segSimilarityWanted = similarWanted,
                                        segSimilarityCurrent = 0.0,
                                        segRng = rng }


coordToAid :: (Int, Int) -> SegCoord -> AgentId
coordToAid (xMax, yMax) (x, y) = (y * xMax) + x
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoAfterTime = segDt ao 1.0
        returnA -< aoAfterTime