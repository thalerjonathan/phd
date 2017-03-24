{-# LANGUAGE Arrows #-}
module Segregation.SegregationModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SegParty = Red | Green deriving (Eq, Show)
type SegMsg = ()    -- Agents are not communicating in Schelling Segregation

data SegMoveStrategy = MoveLocal Int | MoveGlobal deriving (Eq)
data SegSelectionStrategy = SelectNearest | SelectRandom Int Int deriving (Eq)
data SegOptStrategy = OptNone
                        | OptSimilaritySatisfied
                        | OptSimilarityIncreasing
                            deriving (Eq)

data SegAgentState = SegAgentState {
    segParty :: SegParty,
    segSatisfactionLevel :: Double,
    segSatisfactionWanted :: Double,
    segRng :: StdGen
} deriving (Show)

type SegEnvCell = Maybe SegParty
type SegEnvironment = Environment SegEnvCell

type SegAgentDef = AgentDef SegAgentState SegMsg SegEnvCell
type SegAgentBehaviour = AgentBehaviour SegAgentState SegMsg SegEnvCell
type SegAgentIn = AgentIn SegAgentState SegMsg SegEnvCell
type SegAgentOut = AgentOut SegAgentState SegMsg SegEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
satisfactionWanted :: Double
satisfactionWanted = 0.75

density :: Double
density = 0.75

redGreenDist :: Double
redGreenDist = 0.5

localMovementRadius :: Int
localMovementRadius = 5

randomSearchFreeCellRetries :: Int
randomSearchFreeCellRetries = 4

randomSearchOptRetries :: Int
randomSearchOptRetries = 4

movementStrategy :: SegMoveStrategy
movementStrategy = MoveGlobal -- MoveLocal localMovementRadius

selectionStrategy :: SegSelectionStrategy
selectionStrategy = SelectNearest -- SelectRandom randomSearchOptRetries randomSearchFreeCellRetries

optimizingStrategy :: SegOptStrategy
optimizingStrategy = OptSimilarityIncreasing -- OptNone -- OptSimilarityIncreasing
------------------------------------------------------------------------------------------------------------------------

recTracingAgentId = 1

------------------------------------------------------------------------------------------------------------------------
-- HELPER-Functions
------------------------------------------------------------------------------------------------------------------------
isParty :: SegAgentOut -> SegParty -> Bool
isParty ao p = (segParty s) == p
    where
        s = aoState ao

isOccupied :: SegEnvCell -> Bool
isOccupied = isJust

isSatisfied :: SegAgentOut -> Bool
isSatisfied aout = (segSatisfactionLevel s) >= (segSatisfactionWanted s)
    where
        s = aoState aout
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
-- TODO: need to distinguish if we are in recursion or not: if we accept happy immediately, then we cannot stop recursion and can't distinguish between multiple happiness
segMovement :: SegAgentOut -> SegAgentIn -> Double -> SegAgentOut
segMovement ao ain dt
    | isSatisfied ao' = ao'
    | otherwise = move ao' ain
    where
         -- NOTE: need to re-calculate similarity and update agent because could have changed since last update
        ao' = updateSatisfactionLevel ao

updateSatisfactionLevel :: SegAgentOut -> SegAgentOut
updateSatisfactionLevel ao = updateState ao (\s -> s { segSatisfactionLevel = updatedSatisfactionLevel } )
    where
        coord = aoEnvPos ao
        updatedSatisfactionLevel = calculateSatisfactionOn ao coord True

calculateSatisfactionOn :: SegAgentOut -> EnvCoord -> Bool -> Double
calculateSatisfactionOn ao coord includeSelf
    | isParty ao Red = (fromInteger $ fromIntegral reds + inc) / totalCount
    | isParty ao Green = (fromInteger $ fromIntegral greens + inc) / totalCount
    where
        s = aoState ao
        env = aoEnv ao
        cs = neighbours env coord
        inc = if includeSelf then 1 else 0
        (reds, greens) = countOccupied cs
        totalCount = (fromInteger $ fromIntegral (reds + greens))

        countOccupied :: [SegEnvCell] -> (Int, Int)
        countOccupied cs = (redCount, greenCount)
            where
                occupiedCells = filter isOccupied cs
                redCount = length $ filter ((==Red) . fromJust) occupiedCells
                greenCount = length $ filter ((==Green) . fromJust) occupiedCells


move :: SegAgentOut -> SegAgentIn -> SegAgentOut
move ao ain = maybe ao' (\optCoord -> moveTo ao' optCoord) mayOptCoord
    where
        (ao', mayOptCoord) = findMove
                                optimizingStrategy
                                movementStrategy
                                selectionStrategy
                                ao

moveTo :: SegAgentOut -> EnvCoord -> SegAgentOut
moveTo ao newCoord = ao'
    where
        s = aoState ao
        env = aoEnv ao
        oldCoord = aoEnvPos ao
        c = segParty s
        env' = changeCellAt env oldCoord Nothing
        env'' = changeCellAt env' newCoord (Just c)
        ao' = ao { aoEnv = env'', aoEnvPos = newCoord }

findMove :: SegOptStrategy
                -> SegMoveStrategy
                -> SegSelectionStrategy
                -> SegAgentOut
                -> (SegAgentOut, Maybe EnvCoord)

findMove opts ms SelectNearest ao = (ao, findNearestFreeCoord ao (optimizeDistance opts ao) ms)
findMove opts ms (SelectRandom optRetries freeCellRetries) ao = findOptRandomFreeCoord ao ms optRetries freeCellRetries (optimizeSatisfaction opts)

------------------------------------------------------------------------------------------------------------------------
-- OPTIMIZING
------------------------------------------------------------------------------------------------------------------------

moveAlways :: (SegAgentOut -> EnvCoord -> Bool)
moveAlways _ _ = True


moveSatisfies :: (SegAgentOut -> EnvCoord -> Bool)
moveSatisfies ao coord = similiarityOnCoord > (segSatisfactionWanted (aoState ao))
    where
        similiarityOnCoord = calculateSatisfactionOn ao coord True

moveImproves :: (SegAgentOut -> EnvCoord -> Bool)
moveImproves ao coord = satisfactionLevelOnCoord > satisfactionLevel
    where
        satisfactionLevelOnCoord = calculateSatisfactionOn ao coord True
        satisfactionLevel = (segSatisfactionLevel (aoState ao))

optimizeSatisfaction :: SegOptStrategy -> SegAgentOut -> EnvCoord -> Bool
optimizeSatisfaction OptNone = moveAlways
optimizeSatisfaction OptSimilaritySatisfied = moveSatisfies
optimizeSatisfaction OptSimilarityIncreasing = moveImproves

optimizeDistance :: SegOptStrategy
                        -> SegAgentOut
                        -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell)
optimizeDistance opts ao best@(bestCoord, bestCell) comp@(compCoord, compCell) = if distComp < distBest then
                                                                                     if improves then
                                                                                        comp
                                                                                        else
                                                                                            best
                                                                                     else
                                                                                         best

    where
        agentPos = aoEnvPos ao
        improves = optimizeSatisfaction opts ao compCoord
        distBest = distance agentPos bestCoord
        distComp = distance agentPos compCoord
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Nearest Cell Selection
------------------------------------------------------------------------------------------------------------------------
findNearestFreeCoord :: SegAgentOut
                        -> ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                        -> SegMoveStrategy
                        -> Maybe EnvCoord
findNearestFreeCoord ao optFunc (MoveLocal radius) = findNearestLocal ao optFunc
findNearestFreeCoord ao optFunc MoveGlobal = findNearestGlobal ao optFunc

findNearestGlobal :: SegAgentOut
                        -> ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                        -> Maybe EnvCoord
findNearestGlobal ao optFunc = findNearestGlobalAux 0 ao
    where
        findNearestGlobalAux :: Int -> SegAgentOut -> Maybe EnvCoord
        findNearestGlobalAux scale ao
            | isJust mayNearest = Just $ fst $ fromJust mayNearest
            | otherwise = findNearestGlobalAux (scale + 1) ao
            where
                env = aoEnv ao
                coord = aoEnvPos ao
                l = envLimits env
                w = envWrapping env
                mooreRingCoords = neighbourhoodScale mooreSelf scale
                mooreRingNeighbourhood = neighbourhoodOf coord mooreRingCoords
                wrappedMooreRingNeighbourhood = wrapNeighbourhood l w mooreRingNeighbourhood
                mooreRingCells = cellsAt env wrappedMooreRingNeighbourhood
                mayNearest = foldr (findNearestAux optFunc) Nothing (zip wrappedMooreRingNeighbourhood mooreRingCells)

findNearestLocal :: SegAgentOut
                        -> ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                        -> Maybe EnvCoord
findNearestLocal ao optFunc
    | isJust mayNearest = Just $ fst $ fromJust mayNearest
    | otherwise = Nothing
    where
        env = aoEnv ao
        coord = aoEnvPos ao
        -- TODO: this includes itself, is a problem when calcuulating if a move makes happy: if one checks the position the agent is on, it has heavier weight because there is +1 included
        coordsCells = cellsAround env coord localMovementRadius
        mayNearest = foldr (findNearestAux optFunc) Nothing coordsCells

findNearestAux :: ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                    -> (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
findNearestAux optFunc comp@(compCoord, compCell) Nothing
    | isOccupied compCell = Nothing
    | otherwise = Just comp
findNearestAux optFunc comp@(compCoord, compCell) best@(Just (bestCoord, bestCell))
    | isOccupied compCell = best
    | otherwise = Just $ optFunc (bestCoord, bestCell) comp
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Random Cell Selection
------------------------------------------------------------------------------------------------------------------------
findOptRandomFreeCoord :: SegAgentOut
                            -> SegMoveStrategy
                            -> Int
                            -> Int
                            -> (SegAgentOut -> EnvCoord -> Bool)
                             -> (SegAgentOut, Maybe EnvCoord)
findOptRandomFreeCoord ao ms 0 freeCellRetries optFunc = findRandomFreeCoord ao ms freeCellRetries
findOptRandomFreeCoord ao ms optRetries freeCellRetries optFunc
    | freeCoordFound = if optFunc ao' freeCoord then ret else findOptRandomFreeCoord ao' ms (optRetries - 1) freeCellRetries optFunc
    | otherwise = ret
    where
        ret@(ao', mayFreeCoord) = findRandomFreeCoord ao ms freeCellRetries
        freeCoordFound = isJust mayFreeCoord
        freeCoord = fromJust mayFreeCoord

findRandomFreeCoord :: SegAgentOut -> SegMoveStrategy -> Int -> (SegAgentOut, Maybe EnvCoord)
findRandomFreeCoord ao ms 0 = (ao, Nothing)
findRandomFreeCoord ao ms maxRetries
    | isOccupied randCell = findRandomFreeCoord ao' ms (maxRetries - 1)
    | otherwise = (ao', Just randCoord)
    where
        (ao', randCell, randCoord) = findRandomFreeCoordAux ms ao

        findRandomFreeCoordAux :: SegMoveStrategy -> SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
        findRandomFreeCoordAux (MoveLocal radius) ao = localRandomCell ao radius
        findRandomFreeCoordAux MoveGlobal ao = globalRandomCell ao

localRandomCell :: SegAgentOut -> Int -> (SegAgentOut, SegEnvCell, EnvCoord)
localRandomCell ao radius = (ao', randCell, randCoord)
    where
        s = aoState ao
        env = aoEnv ao
        g = segRng s
        originCoord = aoEnvPos ao
        (randCell, randCoord, g') = randomCellWithRadius g env originCoord radius
        ao' = updateState ao (\s -> s { segRng = g' } )

globalRandomCell :: SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
globalRandomCell ao = (ao', randCell, randCoord)
    where
        s = aoState ao
        env = aoEnv ao
        g = segRng s
        (randCell, randCoord, g') = randomCell g env
        ao' = updateState ao (\s -> s { segRng = g' } )
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< segMovement ao ain 1.0