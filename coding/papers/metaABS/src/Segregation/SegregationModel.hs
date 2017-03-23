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

data SegMoveStrategy = Local | Global deriving (Eq)
data SegOptStrategy = None
                        | NearestMakesHappy
                        | MakesHappy Int
                        | OptimizePresent Int
                        | OptimizeRecursive Int Int
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

freeCellRetries :: Int
freeCellRetries = 4

localMovementRadius :: Int
localMovementRadius = 5

movementStrategy :: SegMoveStrategy
movementStrategy = Local

optimizingStrategy :: SegOptStrategy
optimizingStrategy =  None -- None -- NearestMakesHappy -- MakesHappy 4 -- OptimizePresent 1 -- OptimizeRecursive 1 1 1
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

type MoveImprovesFunc = (SegAgentOut -> EnvCoord -> Bool)
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
        updatedSatisfactionLevel = calculateSimilarity ao
        ao' = updateState ao (\s -> s { segSatisfactionLevel = updatedSatisfactionLevel } )

move :: SegAgentOut -> SegAgentIn -> SegAgentOut
move ao ain
    | optCoordFound = moveTo ao' optCoord
    | otherwise = ao'
    where
        (ao', mayOptCoord) = findOptMove optimizingStrategy ao ain
        optCoordFound = isJust mayOptCoord
        optCoord = fromJust mayOptCoord

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

findOptMove :: SegOptStrategy -> SegAgentOut -> SegAgentIn -> (SegAgentOut, Maybe EnvCoord)
findOptMove None ao _ = findFreeCoord ao freeCellRetries
findOptMove (NearestMakesHappy) ao _ = (ao, findNearest ao)
findOptMove (MakesHappy retries) ao _ = findOptMoveAux ao retries moveMakesHappy
findOptMove (OptimizePresent retries) ao _ = findOptMoveAux ao retries moveImproves
findOptMove (OptimizeRecursive depth retries) ao ain
    | isRecursive ain = trace ("recOuts " ++ (show $ (map aoState recOuts)))
                            (if length recOuts < 5 then
                                (recAout, mayFreeCoord)
                                else
                                    (ao', Nothing))

    | otherwise = if (aoId ao == recTracingAgentId) then (recAout, mayFreeCoord) else ret
    where
        recInputEvent = aiRec ain
        recOuts = fromEvent $ recInputEvent

        ret@(ao', mayFreeCoord) = findFreeCoord ao freeCellRetries
        recAout = recursive ao'

findOptMoveAux :: SegAgentOut -> Int -> MoveImprovesFunc -> (SegAgentOut, Maybe EnvCoord)
findOptMoveAux ao 0 optFunc = findFreeCoord ao freeCellRetries
findOptMoveAux ao retries optFunc
    | freeCoordFound = if optFunc ao' freeCoord then ret else findOptMoveAux ao' (retries - 1) optFunc
    | otherwise = ret
    where
        ret@(ao', mayFreeCoord) = findFreeCoord ao freeCellRetries
        freeCoordFound = isJust mayFreeCoord
        freeCoord = fromJust mayFreeCoord

findNearest :: SegAgentOut -> Maybe EnvCoord
findNearest ao
    | isJust mayNearest = Just $ fst $ fromJust mayNearest
    | otherwise = Nothing
    where
        env = aoEnv ao
        coord = aoEnvPos ao
        -- TODO: this includes itself, is a problem when calcuulating if a move makes happy: if one checks the position the agent is on, it has heavier weight because there is +1 included
        coordsCells = cellsAround env coord localMovementRadius
        mayNearest = foldr (findNearestAux coord ao) Nothing coordsCells

        findNearestAux :: EnvCoord -> SegAgentOut -> (EnvCoord, SegEnvCell) -> Maybe (EnvCoord, SegEnvCell) -> Maybe (EnvCoord, SegEnvCell)
        findNearestAux _ _ p@(coord, c) Nothing
            | isOccupied c = Nothing
            | otherwise = Just p
        findNearestAux agentPos ao p@(pairCoord, c) best@(Just (bestCoord, bestCell))
            | isOccupied c = best
            | otherwise = if not pairMakesHappy then
                            best
                            else
                               if distPair < distBest then
                                    Just p
                                    else
                                        best

            where
                distBest = distance agentPos bestCoord
                distPair = distance agentPos pairCoord
                bestMakesHappy = moveMakesHappy ao bestCoord
                pairMakesHappy = moveMakesHappy ao pairCoord

moveImproves :: MoveImprovesFunc
moveImproves ao coord = satisfactionLevelOnCoord > satisfactionLevel
    where
        satisfactionLevelOnCoord = calculateSimilarityOn ao coord True
        satisfactionLevel = (segSatisfactionLevel (aoState ao))

moveMakesHappy :: MoveImprovesFunc
moveMakesHappy ao coord = similiarityOnCoord > (segSatisfactionWanted (aoState ao))
    where
        similiarityOnCoord = calculateSimilarityOn ao coord True

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
        originCoord = aoEnvPos ao
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

calculateSimilarity :: SegAgentOut -> Double
calculateSimilarity ao = calculateSimilarityOn ao coord True
    where
        coord = aoEnvPos ao

calculateSimilarityOn :: SegAgentOut -> EnvCoord -> Bool -> Double
calculateSimilarityOn ao coord includeSelf
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


segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< segMovement ao ain 1.0