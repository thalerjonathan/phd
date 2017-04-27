{-# LANGUAGE Arrows #-}
module Segregation.SegregationModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
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
    segSimilarityWanted :: Double
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
------------------------------------------------------------------------------------------------------------------------
similarityWanted :: Double
similarityWanted = 0.75

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
optimizingStrategy = OptSimilaritySatisfied -- OptNone -- OptSimilaritySatisfied -- OptSimilarityIncreasing 

futureOptimizing :: Bool
futureOptimizing = True
------------------------------------------------------------------------------------------------------------------------

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
isSatisfied aout = (segSatisfactionLevel s) >= (segSimilarityWanted s)
    where
        s = aoState aout
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
-- the recursive case: only move to the target point when it satisfies ourself in the present and in the future
    -- satisfied in the present: stay & check if satisfied in future
            -- satisfied in future: stay
            -- NOT satsfied in future: move

    -- not satisfied in the present: move and don't care about future

-- in the recursive case we have the SegAgentOut of the present which initiated the recursion AND the SegAgentOut of the future
-- in the recursive case (future) we need to decide the above cases and return the corresponding SegAgentOut
-- IMPORTANT: we always need to return the ENVIRONMENT of the PRESENT SegAgentOut
segMovementRec :: SegAgentIn -> SegAgentOut -> SegAgentOut
segMovementRec ain ao 
    | isRecursive ain = unrecursive aoFinal
    | otherwise = if (isSatisfied ao') then recursive ao' False else ao'    -- satisfied in the present: check for future, otherwise don't care for future and just move
    where
        -- NOTE: this is the decision in the present (non-recursive)
        ao' = segMovement ao

        -- NOTE: we know that the present IS SATISFIED given is environment and that is HAS NOT MOVED

        -- NOTE: the out from the origin in is the present because it is the one returned in the case of not being in the recursion
        aoPresent = ao
        -- NOTE: this is the same as present
        aoPresent' = head $ fromEvent $ aiRec ain
        -- NOTE: this becomes the future one by calculating the satisfaction-level which depends on the environment which was changed
        -- by the other agents which were simulated before
        aoFuture = updateSatisfactionLevel ao
        -- NOTE: the final output is: in case we are happy in the future as well then the present position unchanged, 
        -- if we are not happy in the future we move in the present
        aoFinal = if (isSatisfied aoFuture) then aoPresent else move aoPresent

        segMovementPresent :: SegAgentOut -> SegAgentOut
        segMovementPresent ao
            | isSatisfied ao' = recursive ao' False
            | otherwise = move ao'
            where

segMovement :: SegAgentOut -> SegAgentOut
segMovement ao 
    | isSatisfied ao' = ao'
    | otherwise = move ao'
    where
        -- NOTE: need to re-calculate similarity and update agent because could have changed since last update
        ao' = updateSatisfactionLevel ao

updateSatisfactionLevel :: SegAgentOut -> SegAgentOut
updateSatisfactionLevel ao = updateState ao (\s -> s { segSatisfactionLevel = updatedSatisfactionLevel } )
    where
        agentPos = aoEnvPos ao
        updatedSatisfactionLevel = satisfactionOn ao agentPos

-- NOTE: this function calculates the projected satisfaction of the party if the given coordinate is occupied by this party

satisfactionOn :: SegAgentOut -> EnvCoord -> Double
satisfactionOn ao targetCoord
    | Red == party = redFraction
    | otherwise = greenFraction
    where
        env = aoEnv ao
        party = segParty $ aoState ao
        cs = neighbours env targetCoord
        (reds, greens) = countOccupied cs

        -- NOTE: if the agent is in the neighbourhood then don't include itself, otherwise include itself (although it might be on the targetcoord, the targetCoord itself is not included)
        includeAgent = if isAgentInNeighbourhood ao cs then 0 else 1
        totalCount = (fromInteger $ fromIntegral (reds + greens + includeAgent))
        redFraction = (fromInteger $ fromIntegral (reds + includeAgent)) / totalCount
        greenFraction = (fromInteger $ fromIntegral (greens + includeAgent)) / totalCount

        isAgentInNeighbourhood :: SegAgentOut -> [(EnvCoord, SegEnvCell)] -> Bool
        isAgentInNeighbourhood ao cs = any ((==(agentPos)) . fst) cs
            where
                agentPos = aoEnvPos ao

        countOccupied :: [(EnvCoord, SegEnvCell)] -> (Int, Int)
        countOccupied cs = (redCount, greenCount)
            where
                occupiedCells = filter (isOccupied . snd) cs
                redCount = length $ filter ((==Red) . fromJust . snd) occupiedCells
                greenCount = length $ filter ((==Green) . fromJust . snd) occupiedCells


move :: SegAgentOut -> SegAgentOut
move ao = maybe ao' (\optCoord -> moveTo ao' optCoord) mayOptCoord
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
findMove opts ms SelectNearest ao = (ao,
                                        findNearestFreeCoord
                                        ao
                                        (optimizeDistance opts ao)
                                        ms)
findMove opts ms (SelectRandom optRetries freeCellRetries) ao = findOptRandomFreeCoord
                                                                    ao
                                                                    ms
                                                                    optRetries
                                                                    freeCellRetries
                                                                    (optimizeSatisfaction opts)
------------------------------------------------------------------------------------------------------------------------
-- OPTIMIZING
------------------------------------------------------------------------------------------------------------------------
moveAlways :: SegAgentOut -> EnvCoord -> Bool
moveAlways _ _ = True

moveSatisfies :: SegAgentOut -> EnvCoord -> Bool
moveSatisfies ao targetCoord = similiarityOnCoord > (segSimilarityWanted (aoState ao))
    where
        similiarityOnCoord = satisfactionOn ao targetCoord

moveImproves :: SegAgentOut -> EnvCoord -> Bool
moveImproves ao targetCoord = satisfactionLevelOnCoord > satisfactionLevel
    where
        satisfactionLevelOnCoord = satisfactionOn ao targetCoord
        satisfactionLevel = (segSatisfactionLevel (aoState ao))

optimizeSatisfaction :: SegOptStrategy -> SegAgentOut -> EnvCoord -> Bool
optimizeSatisfaction OptNone = moveAlways
optimizeSatisfaction OptSimilaritySatisfied = moveSatisfies
optimizeSatisfaction OptSimilarityIncreasing = moveImproves

optimizeDistance :: SegOptStrategy
                        -> SegAgentOut
                        -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell)
optimizeDistance opts ao best@(bestCoord, _) comp@(compCoord, _) = if distComp < distBest then
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
findNearestFreeCoord ao optFunc (MoveLocal _) = findNearestLocal ao optFunc
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
        coordsCells = cellsAround env coord localMovementRadius
        mayNearest = foldr (findNearestAux optFunc) Nothing coordsCells

findNearestAux :: ((EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell) -> (EnvCoord, SegEnvCell))
                    -> (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
                    -> Maybe (EnvCoord, SegEnvCell)
findNearestAux _ comp@(_, compCell) Nothing
    | isOccupied compCell = Nothing
    | otherwise = Just comp
findNearestAux optFunc comp@(_, compCell) best@(Just (bestCoord, bestCell))
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
findOptRandomFreeCoord ao ms 0 freeCellRetries _ = findRandomFreeCoord ao ms freeCellRetries
findOptRandomFreeCoord ao ms optRetries freeCellRetries optFunc =
        maybe ret (\freeCoord -> if optFunc ao' freeCoord then
                                    ret
                                    else
                                        findOptRandomFreeCoord ao' ms (optRetries - 1) freeCellRetries optFunc) mayFreeCoord
    where
        ret@(ao', mayFreeCoord) = findRandomFreeCoord ao ms freeCellRetries

findRandomFreeCoord :: SegAgentOut -> SegMoveStrategy -> Int -> (SegAgentOut, Maybe EnvCoord)
findRandomFreeCoord ao _ 0 = (ao, Nothing)
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
        env = aoEnv ao
        originCoord = aoEnvPos ao
        ((randCell, randCoord), ao') = runAgentRandom ao (randomCellWithRadius env originCoord radius)

globalRandomCell :: SegAgentOut -> (SegAgentOut, SegEnvCell, EnvCoord)
globalRandomCell ao = (ao', randCell, randCoord)
    where
        env = aoEnv ao
        ((randCell, randCoord), ao') = runAgentRandom ao (randomCell env)
------------------------------------------------------------------------------------------------------------------------

segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoMoved = if futureOptimizing then segMovementRec ain ao else segMovement ao
        returnA -< aoMoved