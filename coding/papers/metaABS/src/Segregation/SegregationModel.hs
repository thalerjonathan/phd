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
data SegAgentType = Red | Green deriving (Eq, Show)
type SegMsg = ()    -- Agents are not communicating in Schelling Segregation

data SegMoveStrategy = Local | Global deriving (Eq)
data SegOptStrategy = None
                        | NearestMakesHappy
                        | MakesHappy Int
                        | OptimizePresent Int
                        | OptimizeRecursive Int Int
                            deriving (Eq)

type SegCoord = (Int, Int)

-- TODO: move 2d discrete coordinates to env / agent instead of segagentstate: map of agentid to coords? or spatialinfo in agentin/out?
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

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
similarWanted :: Double
similarWanted = 0.8

density :: Double
density = 0.75

redGreenDist :: Double
redGreenDist = 0.5

freeCellRetries :: Int
freeCellRetries = 4

localMovementRadius :: Int
localMovementRadius = 2

movementStrategy :: SegMoveStrategy
movementStrategy = Local

optimizingStrategy :: SegOptStrategy
optimizingStrategy =  MakesHappy 1 -- None -- NearestMakesHappy -- MakesHappy 4 -- OptimizePresent 1 -- OptimizeRecursive 1 1 1
------------------------------------------------------------------------------------------------------------------------

recTracingAgentId = 1

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
is :: SegAgentOut -> SegAgentType -> Bool
is ao sat = (segAgentType s) == sat
    where
        s = aoState ao

isOccupied :: SegEnvCell -> Bool
isOccupied = isJust

isHappy :: SegAgentOut -> Bool
isHappy aout = (segSimilarityCurrent s) >= (segSimilarityWanted s)
    where
        s = aoState aout

-- TODO: need to distinguish if we are in recursion or not: if we accept happy immediately, then we cannot stop recursion and can't distinguish between multiple happiness
segMovement :: SegAgentOut -> SegAgentIn -> Double -> SegAgentOut
segMovement aout ain dt
    | isHappy aout' = aout'
    | otherwise = move aout' ain
    where
         -- NOTE: need to re-calculate similarity and update agent because could have changed since last update
        s = aoState aout
        coord = segCoord s
        updatedCurrentSimilarity = calculateSimilarity aout
        aout' = updateState aout (\s -> s { segSimilarityCurrent = updatedCurrentSimilarity } )

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
findOptMove (NearestMakesHappy) aout _ = (aout, findNearest aout)
findOptMove (MakesHappy retries) aout _ = findOptMoveAux aout retries moveMakesHappy
findOptMove (OptimizePresent retries) aout _ = findOptMoveAux aout retries moveImproves
findOptMove (OptimizeRecursive depth retries) aout ain
    | isRecursive ain = trace ("recOuts " ++ (show $ (map aoState recOuts)))
                            (if length recOuts < 5 then
                                (recAout, mayFreeCoord)
                                else
                                    (aout', Nothing))

    | otherwise = if (aoId aout == recTracingAgentId) then (recAout, mayFreeCoord) else ret
    where
        recInputEvent = aiRec ain
        recOuts = fromEvent $ recInputEvent

        ret@(aout', mayFreeCoord) = findFreeCoord aout freeCellRetries
        recAout = recursive aout'

findNearest :: SegAgentOut -> Maybe EnvCoord
findNearest aout
    | isJust mayNearest = Just $ fst $ fromJust mayNearest
    | otherwise = Nothing
    where
        s = aoState aout
        env = aoEnv aout
        coord = segCoord s
        -- TODO: this includes itself, is a problem when calcuulating if a move makes happy: if one checks the position the agent is on, it has heavier weight because there is +1 included
        coordsCells = cellsAround env coord localMovementRadius
        mayNearest = foldr (findNearestAux coord aout) Nothing coordsCells

        findNearestAux :: EnvCoord -> SegAgentOut -> (EnvCoord, SegEnvCell) -> Maybe (EnvCoord, SegEnvCell) -> Maybe (EnvCoord, SegEnvCell)
        findNearestAux _ _ p@(coord, c) Nothing
            | isOccupied c = Nothing
            | otherwise = Just p
        findNearestAux agentPos aout p@(pairCoord, c) best@(Just (bestCoord, bestCell))
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
                bestMakesHappy = moveMakesHappy aout bestCoord
                pairMakesHappy = moveMakesHappy aout pairCoord

findOptMoveAux :: SegAgentOut -> Int -> MoveImprovesFunc -> (SegAgentOut, Maybe EnvCoord)
findOptMoveAux ao 0 optFunc = findFreeCoord ao freeCellRetries
findOptMoveAux ao retries optFunc
    | freeCoordFound = if optFunc ao' freeCoord then ret else findOptMoveAux ao' (retries - 1) optFunc
    | otherwise = ret
    where
        ret@(ao', mayFreeCoord) = findFreeCoord ao freeCellRetries
        freeCoordFound = isJust mayFreeCoord
        freeCoord = fromJust mayFreeCoord

type MoveImprovesFunc = (SegAgentOut -> EnvCoord -> Bool)

moveImproves :: MoveImprovesFunc
moveImproves ao coord = similiarityOnCoord > similiarityCurrent
    where
        similiarityOnCoord = calculateSimilarityOn ao coord True
        similiarityCurrent = (segSimilarityCurrent (aoState ao))

moveMakesHappy :: MoveImprovesFunc
moveMakesHappy ao coord = similiarityOnCoord > (segSimilarityWanted (aoState ao))
    where
        similiarityOnCoord = calculateSimilarityOn ao coord True

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
        ao'' = updateState ao' (\s -> s { segCoord = newCoord } )
        --newSimilarity = calculateSimilarity ao'
        --ao''' = updateState ao'' (\s -> s { segSimilarityCurrent = newSimilarity } )

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

calculateSimilarity :: SegAgentOut -> Double
calculateSimilarity ao = calculateSimilarityOn ao coord False
    where
        s = aoState ao
        coord = segCoord s

calculateSimilarityOn :: SegAgentOut -> SegCoord -> Bool -> Double
calculateSimilarityOn ao coord includeSelf
    | is ao Red = (fromInteger $ fromIntegral reds + inc) / totalCount
    | is ao Green = (fromInteger $ fromIntegral greens + inc) / totalCount
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