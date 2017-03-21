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
data SegOptStrategy = None | OptimizePresent Int | OptimizeRecursive Int Int deriving (Eq)

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

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
similarWanted :: Double
similarWanted = 0.75

density :: Double
density = 0.75

redGreenDist :: Double
redGreenDist = 0.5

freeCellRetries :: Int
freeCellRetries = 3

localMovementRadius :: Int
localMovementRadius = 5

movementStrategy :: SegMoveStrategy
movementStrategy = Local

optimizingStrategy :: SegOptStrategy
optimizingStrategy = None -- None -- OptimizeRecursive 1 1 4 --  OptimizePresent 4   OptimizeRecursive 1 1 4
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

segMovement :: SegAgentOut -> SegAgentIn -> Double -> SegAgentOut
segMovement aout ain dt
    | isHappy aout = if (aiId ain == recTracingAgentId) then trace ("agentid 1 is happy") aout else aout
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

{-
findOptMove (OptimizeRecursive depth retries) aout ain
    | isRecursive ain = handleRecursion aout ain
    | otherwise = recursiveMove aout
    where
        recursiveMove :: SegAgentOut -> (SegAgentOut, Maybe EnvCoord)
        recursiveMove aout = maybe
                                (aout', mayFreeCoord)
                                (\_ -> if (aoId aout == recTracingAgentId) then
                                            trace ("starting recursion with " ++ show (fromJust mayFreeCoord)) (recursiveAout, mayFreeCoord)
                                            else
                                                (recursiveAout, mayFreeCoord) )
                                mayFreeCoord
            where
                ret@(aout', mayFreeCoord) = findFreeCoord aout freeCellRetries
                -- NOTE: for now only the agent with id 0 performs recursive simulation, otherwise can't debug
                recursiveAout = if (aoId aout == recTracingAgentId) then recursive aout' else aout'

        handleRecursion :: SegAgentOut -> SegAgentIn -> (SegAgentOut, Maybe EnvCoord)
        handleRecursion aout ain = trace ("agent has recursion: agentid = "
                    ++ (show (aiId ain))
                    ++ "\n currentState = "
                    ++ (show $ aoState aout)
                    ++ "\n futureOut = "
                    ++ (show $ (map aoState pastOuts))) ret
            where
                recEvent = aiRec ain
                pastOuts = fromEvent $ recEvent
                ret = recursiveMove aout

                {-
                -- TODO: recalculate future similarity: environment has changed

                currentSimiliarity = (segSimilarityCurrent (aoState aout))
                aoutNoRec = unrecursive aout

                ret = if (futureSimiliarity > currentSimiliarity) then
                        trace ("future move of " ++ (show $ futureCoord) ++ " increases similarity") (aoutNoRec, Just futureCoord)
                        else
                            trace ("future move of " ++ (show $ futureCoord) ++ " decreases similarity, continue recursion") (recursiveMove aout)
                -}
-}

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

calculateHappinessChange :: [SegAgentOut] -> [SegAgentOut] -> Double
calculateHappinessChange aoutsPrev aoutCurr = sumSimilarityWantedCurr - sumSimilarityWantedPrev
    where
        sumSimilarityWantedPrev = sum $ map (segSimilarityCurrent . aoState) aoutsPrev
        sumSimilarityWantedCurr = sum $ map (segSimilarityCurrent . aoState) aoutCurr

calculateStats :: [SegAgentOut] -> (Int, Int, Int, Double)
calculateStats aouts = (totalCount, happyCount, unhappyCount, unhappyFract)
    where
        totalCount = length aouts
        happy = filter isHappy aouts
        happyCount = length happy
        unhappyCount = totalCount - happyCount
        unhappyFract = (fromInteger $ fromIntegral unhappyCount) / (fromInteger $ fromIntegral totalCount)
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


segAgentBehaviour :: SegAgentBehaviour
segAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< segMovement ao ain 1.0