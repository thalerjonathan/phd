{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeAgent where

-- Project-internal import first
import SugarScape.SugarScapeModel
import SugarScape.SugarScapeEnvironment
import FrABS.Env.Environment
import FrABS.Agent.Agent

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentDies :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDies = unoccupyPosition . kill

passWealthOn :: SugarScapeAgentOut -> SugarScapeAgentOut
passWealthOn a
    | null childrenIds = a
    | otherwise = broadcastMessage a (InheritSugar childrenSugarShare) childrenIds
    where
        s = aoState a
        sugarLevel = sugAgSugarLevel s

        childrenIds = sugAgChildren s
        childrenCount = length childrenIds
        childrenSugarShare = sugarLevel / (fromRational $ toRational $ fromIntegral childrenCount)

agentAction :: SugarScapeAgentOut -> SugarScapeAgentOut
agentAction a
    | starvedToDeath agentAfterHarvest = agentDies agentAfterHarvest
    | otherwise = agentAfterHarvest
    where
        agentAfterHarvest = agentMetabolism $ agentCollecting a

unoccupyPosition ::  SugarScapeAgentOut -> SugarScapeAgentOut
unoccupyPosition a = a { aoEnv = env' }
    where
        env = aoEnv a

        currentAgentPosition = aoEnvPos a
        currentAgentCell = cellAt env currentAgentPosition
        currentAgentCellUnoccupied = currentAgentCell { sugEnvOccupier = Nothing }

        env' = changeCellAt env currentAgentPosition currentAgentCellUnoccupied

starvedToDeath :: SugarScapeAgentOut -> Bool
starvedToDeath a = sugAgSugarLevel s <= 0
    where
        s = aoState a

agentMetabolism :: SugarScapeAgentOut -> SugarScapeAgentOut
agentMetabolism a = updateState
                            a
                            (\s -> s {
                                sugAgSugarLevel =
                                    max
                                        0
                                        ((sugAgSugarLevel s) - (sugAgMetabolism s))})

agentCollecting :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCollecting a
    | null unoccupiedCells = a
    | otherwise = aHarvested
    where
        cellsInSight = agentLookout a
        unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight

        bestCells = selectBestCells (aoEnvPos a) unoccupiedCells
        -- NOTE: can return equally good cells, do random selection
        (a', cellInfo) = agentPickRandom a bestCells

        aHarvested = agentMoveAndHarvestCell a' cellInfo

agentMoveAndHarvestCell :: SugarScapeAgentOut -> (EnvCoord, SugarScapeEnvCell) -> SugarScapeAgentOut
agentMoveAndHarvestCell a (cellCoord, cell) = updateState a'' (\s -> s { sugAgSugarLevel = newSugarLevelAgent })
    where
        sugarLevelCell = sugEnvSugarLevel cell
        sugarLevelAgent = sugAgSugarLevel $ aoState a
        newSugarLevelAgent = (sugarLevelCell + sugarLevelAgent)

        a' = unoccupyPosition a
        env = aoEnv a'
        s = aoState a'

        --agentMetabolism = sugAgMetabolism $ aoState a
        --polutionIncByMeta =  agentMetabolism * polutionMetabolismFactor
        --polutionIncByHarvest = sugarLevelCell * polutionHarvestFactor
        -- newPolutionLevel = polutionIncByMeta + polutionIncByHarvest + sugEnvPolutionLevel cell
        newPolutionLevel = 0

        cellHarvestedAndOccupied = cell {
                sugEnvSugarLevel = 0.0,
                sugEnvOccupier = Just (cellOccupier (aoId a) s),
                sugEnvPolutionLevel = newPolutionLevel
        }
        env' = changeCellAt env cellCoord cellHarvestedAndOccupied

        a'' = a' { aoEnvPos = cellCoord, aoEnv = env' }


selectBestCells :: EnvCoord -> [(EnvCoord, SugarScapeEnvCell)] -> [(EnvCoord, SugarScapeEnvCell)]
selectBestCells refCoord cs = bestShortestDistanceCells
    where
        measureFunc = bestMeasureSugarLevel

        cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
        bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
        bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

        shortestDistanceBestCells = sortBy (\c1 c2 -> compare (distance refCoord (fst c1)) (distance refCoord (fst c2))) bestCells
        shortestDistance = distance refCoord (fst $ head shortestDistanceBestCells)
        bestShortestDistanceCells = filter ((==shortestDistance) . (distance refCoord) . fst) shortestDistanceBestCells

        bestMeasureSugarLevel :: SugarScapeEnvCell -> Double
        bestMeasureSugarLevel c = sugEnvSugarLevel c

        bestMeasureSugarPolutionRatio :: SugarScapeEnvCell -> Double
        bestMeasureSugarPolutionRatio c = sugLvl / (1 + polLvl)
            where
                sugLvl = sugEnvSugarLevel c
                polLvl = sugEnvPolutionLevel c

-- TODO: think about moving this to the general Agent.hs: introduce a Maybe StdGen, but then: don't we loose reasoning abilities?
-- NOTE: must never be called with empty list
agentPickRandom :: SugarScapeAgentOut -> [a] -> (SugarScapeAgentOut, a)
agentPickRandom a allXs@(x:xs)
    | null xs = (a, x)
    | otherwise = (a', randElem)
    where
        g = sugAgRng $ aoState a
        cellCount = length allXs
        (randIdx, g') = randomR (0, cellCount - 1) g
        randElem = allXs !! randIdx
        a' = updateState a (\s -> s { sugAgRng = g' } )

agentLookout :: SugarScapeAgentOut -> [(EnvCoord, SugarScapeEnvCell)]
agentLookout a = zip visionCoordsWrapped visionCells
    where
        env = aoEnv a
        aPos = aoEnvPos a
        n = envNeighbourhood env
        vis = sugAgVision $ aoState a

        -- TODO: put this logic into environment.hs
        visionCoordsDeltas = foldr (\v acc -> acc ++ (neighbourhoodScale n v)) [] [1 .. vis]
        visionCoords = neighbourhoodOf aPos visionCoordsDeltas
        visionCoordsWrapped = wrapCells (envLimits env) (envWrapping env) visionCoords
        visionCells = cellsAt env visionCoordsWrapped

agentAgeing :: SugarScapeAgentOut -> SugarScapeAgentOut
agentAgeing a
    | dieFromAge a = agentDies $ passWealthOn a -- $ birthNewAgent a
    | otherwise = agentAction a

birthNewAgent :: SugarScapeAgentOut -> SugarScapeAgentOut
birthNewAgent a = createAgent a newAgentDef
    where
        newAgentId = aoId a                                 -- NOTE: we keep the old id
        (newAgentCoord, a') = findUnoccpiedRandomPosition a
        oldAgentRng = sugAgRng $ aoState a'
        (newAgentDef, _) = randomAgent
                            (newAgentId, newAgentCoord)
                            sugarScapeAgentBehaviour
                            sugarScapeAgentConversation
                            oldAgentRng

        findUnoccpiedRandomPosition :: SugarScapeAgentOut -> (EnvCoord, SugarScapeAgentOut)
        findUnoccpiedRandomPosition a
            | cellOccupied c = findUnoccpiedRandomPosition a'
            | otherwise = (coord, a')
            where
                g = sugAgRng $ aoState a
                env = aoEnv a
                (c, coord, g') = randomCell g env
                a' = updateState a (\s -> s { sugAgRng = g' })

dieFromAge :: SugarScapeAgentOut -> Bool
dieFromAge a = age > maxAge
    where
        s = aoState a
        age = sugAgAge s
        maxAge = sugAgMaxAge s
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSex :: SugarScapeAgentOut -> SugarScapeAgentOut
agentSex a
    | isFertile s = agentMatingConversation nids nncsUnoccupied a
    | otherwise = a
    where
        s = aoState a
        pos = aoEnvPos a
        env = aoEnv a

        neighbourCells = neighbours env pos
        nids = neighbourIds a

        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        nncsDupl = foldr (\(coord, _) acc -> (neighbours env coord) ++ acc) neighbourCells neighbourCells
        -- NOTE: the nncs are not unique, remove duplicates
        nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
        nncsUnoccupied = filter (isNothing . sugEnvOccupier . snd) nncsUnique

        agentMatingConversation :: [AgentId]
                                        -> [(EnvCoord, SugarScapeEnvCell)]
                                        -> SugarScapeAgentOut
                                        -> SugarScapeAgentOut
        agentMatingConversation [] _ a = conversationEnd a
        agentMatingConversation _ [] a = conversationEnd a
        agentMatingConversation (receiverId:otherAis) allCoords@((coord, cell):cs) a
            | satisfiesWealthForChildBearing s = conversation a (receiverId, m) agentMatingConversationsReply
            | otherwise = conversationEnd a
            where
                s = aoState a
                m =  MatingRequest (sugAgGender $ s) --trace ("MatingRequest to " ++ (show receiverId)) MatingRequest (sugAgGender $ s)

                agentMatingConversationsReply :: SugarScapeAgentOut
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> SugarScapeAgentOut
                agentMatingConversationsReply a Nothing = agentMatingConversation otherAis allCoords a  -- NOTE: the target was not found or does not have a handler, continue with the next
                agentMatingConversationsReply a (Just (_, MatingReplyNo)) = agentMatingConversation otherAis allCoords a
                agentMatingConversationsReply a (Just (senderId, (MatingReplyYes otherTup))) = 
                    conversation a2 (receiverId, (MatingChild newBornId)) (\a' _ -> agentMatingConversation otherAis cs a')
                    
                    where
                        s = aoState a
                        g = sugAgRng s

                        initialSugarEndow = sugAgSugarInit s
                        sugarLevel = sugAgSugarLevel s

                        mySugarContribution = initialSugarEndow / 2.0
                        myMetab = sugAgMetabolism s
                        myVision = sugAgVision s
                        myCulturalTag = sugAgCulturalTag s

                        newBornId = senderId * aoId a   -- TODO: this is a real problem: which ids do we give our newborns?

                        (newBornDef, g') = createNewBorn
                                                (newBornId, coord)
                                                g
                                                (mySugarContribution, myMetab, myVision, myCulturalTag)
                                                otherTup

                        env = aoEnv a
                        cell' = cell { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef))}
                        env' = changeCellAt env coord cell'

                        a0 = a { aoEnv = env' }
                        a1 = updateState a0 (\s -> s { sugAgSugarLevel = sugarLevel - mySugarContribution,
                                                        sugAgRng = g',
                                                        sugAgChildren = newBornId : (sugAgChildren s)})
                        a2 = createAgent a1 newBornDef
                agentMatingConversationsReply a (Just (_, _)) = agentMatingConversation otherAis allCoords a  -- NOTE: unexpected reply, continue with the next

createNewBorn :: (AgentId, EnvCoord)
                    -> StdGen
                    -> (Double, Double, Int, SugarScapeCulturalTag)
                    -> (Double, Double, Int, SugarScapeCulturalTag)
                    -> (SugarScapeAgentDef, StdGen)
createNewBorn idCoord
                g0
                (sugEndowFather, metabFather, visionFather, cultureFather)
                (sugEndowMother, metabMother, visionMother, cultureMother) = (newBornDef', g4)
    where
        newBornSugarEndow = sugEndowFather + sugEndowMother
        (newBornMetabolism, g1) = crossover (metabFather, metabMother) g0
        (newBornVision, g2) = crossover (visionFather, visionMother) g1
        (newBornCulturalTag, g3) = culturalCrossover cultureFather cultureMother g2

        (newBornDef, g4) = randomAgent
                            idCoord
                            sugarScapeAgentBehaviour
                            sugarScapeAgentConversation
                            g3

        -- TODO: crossover cultural tags

        newBornState = adState newBornDef
        newBornState' = newBornState { sugAgMetabolism = newBornMetabolism,
                                       sugAgVision = newBornVision,
                                       sugAgSugarInit = newBornSugarEndow,
                                       sugAgCulturalTag = newBornCulturalTag,
                                       sugAgTribe = calculateTribe newBornCulturalTag }

        newBornDef' = newBornDef { adState = newBornState' }

        crossover :: (a, a) -> StdGen -> (a, StdGen)
        crossover (x, y) rng
            | takeX = (x, rng')
            | otherwise = (y, rng')
            where
                (takeX, rng') = random rng :: (Bool, StdGen)

satisfiesWealthForChildBearing :: SugarScapeAgentState -> Bool
satisfiesWealthForChildBearing s = currSugar >= initSugar
    where
        currSugar = sugAgSugarLevel s
        initSugar = sugAgSugarInit s

isFertile :: SugarScapeAgentState -> Bool
isFertile s = withinRange age fertilityAgeRange
    where
        age = sugAgAge s
        fertilityAgeRange = sugAgFertAgeRange s

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

inheritSugar :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
inheritSugar ain a = onMessage inheritSugarMatch ain inheritSugarAction a
    where
        inheritSugarAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        inheritSugarAction a (_, (InheritSugar sug)) = updateState a (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + sug})

        inheritSugarMatch :: AgentMessage SugarScapeMsg -> Bool
        inheritSugarMatch (_, InheritSugar _) = True
        inheritSugarMatch _ = False

agentCultureContact :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCultureContact ain a = broadcastMessage a' (CulturalContact culturalTag) nids 
    where
        a' = onMessage cultureContactMatch ain cultureContactAction a
        nids = neighbourIds a'
        culturalTag = sugAgCulturalTag $ aoState a'

        cultureContactMatch :: AgentMessage SugarScapeMsg -> Bool
        cultureContactMatch (_, CulturalContact _) = True
        cultureContactMatch _ = False

        cultureContactAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        cultureContactAction a (_, (CulturalContact tagActive)) = 
                updateState a (\s -> s { sugAgRng = g',
                                         sugAgCulturalTag = tagPassive',
                                         sugAgTribe = tribe})
            where
                s = aoState a
                tagPassive = sugAgCulturalTag s
                g = sugAgRng s
                (tagPassive', g') = cultureContact tagActive tagPassive g
                tribe = calculateTribe tagPassive'

agentCombat :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCombat ain a = a
    where
        s = aoState a
        myTribe = sugAgTribe s
        myWealth = sugAgSugarLevel s 

        cellsInSight = agentLookout a
        cellsOccupied = filter (cellUnoccupied . snd) cellsInSight
        cellsOtherTribe = filter ((/=myTribe) . sugEnvOccTribe . fromJust . sugEnvOccupier . snd) cellsOccupied
        cellsOthersLessWealthy = filter ((<myWealth) . sugEnvOccWealth . fromJust . sugEnvOccupier . snd) cellsOtherTribe
        -- TODO: check if there are other agents of the other tribe which are wealthier in the vision, because this could cause retalation
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- GENERAL AGENT-RELATED
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversation :: SugarScapeAgentConversation
sugarScapeAgentConversation ain (_, (MatingRequest tup)) = (Just m, Just ain')
    where 
        (m, ain') = handleMatingConversation tup ain
sugarScapeAgentConversation ain (_, (MatingChild childId)) = (Nothing, Just ain')
    where
        s = aiState ain
        s' = s { sugAgChildren = childId : (sugAgChildren s)}
        ain' = ain { aiState = s' }
sugarScapeAgentConversation ain _ = (Nothing, Nothing)

handleMatingConversation :: (SugarScapeAgentGender)
                                -> SugarScapeAgentIn
                                -> (SugarScapeMsg, SugarScapeAgentIn)
handleMatingConversation otherGender ain 
    | isFertile s &&
        satisfiesWealthForChildBearing s &&
        differentGender = (MatingReplyYes (mySugarContribution, myMetab, myVision, myCulturalTag), ain')
    | otherwise = (MatingReplyNo, ain)
    where
        s = aiState ain
        myGender = sugAgGender s
        differentGender = myGender /= otherGender

        -- NOTE: to be fertile an agent must have at least as much sugar as initially endowed, therefore it cannot go negative
        initialSugarEndow = sugAgSugarInit s
        sugarLevel = sugAgSugarLevel s
        mySugarContribution = initialSugarEndow / 2.0
        myMetab = sugAgMetabolism s
        myVision = sugAgVision s
        myCulturalTag = sugAgCulturalTag s
        
        s' = s { sugAgSugarLevel = sugarLevel - mySugarContribution }
        ain' = ain { aiState = s'}

neighbourIds :: SugarScapeAgentOut -> [AgentId]
neighbourIds a = map (sugEnvOccId . fromJust . sugEnvOccupier . snd) occupiedCells
    where
        env = aoEnv a
        pos = aoEnvPos a
        neighbourCells = neighbours env pos
        occupiedCells = filter (isJust . sugEnvOccupier . snd) neighbourCells

sugarScapeAgentBehaviour :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviour = proc ain ->
    do
        age <- time -< 0

        let a = agentOutFromIn ain
        let a0 = updateState a (\s -> s { sugAgAge = age })
        let a1 = inheritSugar ain a0
        let a2 = agentCultureContact ain a1
        let a3 = agentCombat ain a2

        let a4 = agentAgeing a3
        let a5 = if isDead a4 then a4 else agentSex a4

        returnA -< a5
------------------------------------------------------------------------------------------------------------------------