{-# LANGUAGE Arrows #-}
module Segregation.Segregation where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

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
data SegOptStrategy = None | OptimizePresent Int | OptimizeRecursive Int Int Int deriving (Eq)

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
type SegAgentIn = AgentIn SegAgentState SegMsg SegEnvCell
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvCell
------------------------------------------------------------------------------------------------------------------------

-- TODO implement recursive simulations: MetaABS: test it using the segregation model. run recursive simulation for local neighbourhood only
    -- for each move compute 1 step recursive and see how this move performs. if it improves the outcome, then take it otherwise stay.

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
similarWanted :: Double
similarWanted = 0.8

density :: Double
density = 0.5

redGreenDist :: Double
redGreenDist = 0.5

freeCellRetries :: Int
freeCellRetries = 3

localMovementRadius :: Int
localMovementRadius = 5

movementStrategy :: SegMoveStrategy
movementStrategy = Local

optimizingStrategy :: SegOptStrategy
optimizingStrategy = OptimizeRecursive 1 1 4 --  OptimizePresent 4   OptimizeRecursive 1 1 4
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

segMovement :: SegAgentOut -> SegAgentIn -> Double -> SegAgentOut
segMovement aout ain dt
    | isHappy aout = aout
    | otherwise = move aout ain

move :: SegAgentOut -> SegAgentIn -> SegAgentOut
move aout ain
    | optCoordFound = moveTo aout' optCoord
    | otherwise = aout'
    where
        (aout', mayOptCoord) = findOptMove optimizingStrategy aout ain
        optCoordFound = isJust mayOptCoord
        optCoord = fromJust mayOptCoord

findOptMove :: SegOptStrategy -> SegAgentOut -> SegAgentIn -> (SegAgentOut, Maybe EnvCoord)
findOptMove None aout _ = findFreeCoord aout freeCellRetries
findOptMove (OptimizePresent retries) aout _ = findOptMoveAux aout retries
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

-- NOTE: a recursive optimizing agent does NOT make an initial move but just requests a recursive simulation
-- TODO: don't move
-- TODO: change optimization technique here or down?
findOptMove (OptimizeRecursive depth steps retries) aout ain = findOptMoveRecursive aout depth steps retries
    where
        findOptMoveRecursive :: SegAgentOut -> Int -> Int -> Int -> (SegAgentOut, Maybe EnvCoord)
        findOptMoveRecursive aout depth steps 0 = findFreeCoord aout freeCellRetries
        findOptMoveRecursive aout depth steps retries
            | freeCoordFound = (recursiveAout, mayFreeCoord)
            | otherwise = ret
            where
                ret@(aout', mayFreeCoord) = findFreeCoord aout freeCellRetries
                freeCoordFound = isJust mayFreeCoord
                freeCoord = fromJust mayFreeCoord
                recursiveAout = recursive aout' depth steps


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

isHappy :: SegAgentOut -> Bool
isHappy ao = (segSimilarityCurrent s) >= (segSimilarityWanted s)
    where
        s = aoState ao
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
                                        adBeh = segAgentBehaviour }
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

handleRecursion :: SegAgentIn -> SegAgentOut
handleRecursion ain = agentOutFromIn ain
    where -- TODO: investigate
        ((totalDepth, currDepth, steps), recAos) = fromEvent $ aiRec ain

handleNonRecursion :: SegAgentIn -> SegAgentOut
handleNonRecursion ain = segMovement ao ain 1.0
    where
        ao = agentOutFromIn ain

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let recEvt = aiRec ain
        let ao = if isEvent recEvt then handleRecursion ain else handleNonRecursion ain
        returnA -< ao