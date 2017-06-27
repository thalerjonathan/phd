{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeAgentMonadic where

import SugarScape.SugarScapeAgentCommon
import SugarScape.SugarScapeModel
import SugarScape.SugarScapeEnvironment
import Utils.Utils

import FrABS.Env.Environment
import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

import FRP.Yampa

import Data.Maybe
import Data.List
import Control.Monad
import Control.Monad.IfElse
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR MONADIC 
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- Chapter II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentDiesM :: State SugarScapeAgentOut ()
agentDiesM = unoccupyPositionM >> killM

unoccupyPositionM :: State SugarScapeAgentOut ()
unoccupyPositionM = 
    do
        (cellCoord, cell) <- agentCellOnPosM
        let cellUnoccupied = cell { sugEnvOccupier = Nothing }
        runEnvironmentM $ changeCellAtM cellCoord cellUnoccupied

passWealthOnM :: State SugarScapeAgentOut ()
passWealthOnM =
    do
        sugarLevel <- domainStateFieldM sugAgSugarLevel
        childrenIds <- domainStateFieldM sugAgChildren

        let hasChildren = (not . null) childrenIds

        when hasChildren $
            do 
                let childrenCount = length childrenIds
                let childrenSugarShare = sugarLevel / (fromRational $ toRational $ fromIntegral childrenCount)
                broadcastMessageM (InheritSugar childrenSugarShare) childrenIds

starvedToDeathM :: State SugarScapeAgentOut Bool
starvedToDeathM = 
    do
        sugar <- domainStateFieldM sugAgSugarLevel
        spice <- domainStateFieldM sugAgSpiceLevel
        return $ (sugar <= 0) || (spice <= 0)


agentMetabolismM :: State SugarScapeAgentOut ()
agentMetabolismM =
    do
        s <- getDomainStateM
        let (sugarMetab, spiceMetab) = metabolismAmount s

        sugarLevel <- domainStateFieldM sugAgSugarLevel
        spiceLevel <- domainStateFieldM sugAgSpiceLevel

        let newSugarLevel = max 0 (sugarLevel - sugarMetab)
        let newSpiceLevel = max 0 (spiceLevel - spiceMetab)

        updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevel, sugAgSpiceLevel = newSpiceLevel })

        -- NOTE: for now the metabolism (and harvest) of spice does not cause any polution
        let pol = sugarMetab * polutionMetabolismFactor
        
        cell <- agentCellOnPosM
        agentPoluteCellM pol cell

        whenM starvedToDeathM agentDiesM

agentPoluteCellM :: Double -> (EnvCoord, SugarScapeEnvCell) -> State SugarScapeAgentOut ()
agentPoluteCellM polutionIncrease (cellCoord, cell)
    | polutionEnabled = 
        do
            let cellAfterPolution = cell { sugEnvPolutionLevel = polutionIncrease + (sugEnvPolutionLevel cell) }
            runEnvironmentM $ changeCellAtM cellCoord cellAfterPolution
    | otherwise = return ()

agentNonCombatMoveM :: State SugarScapeAgentOut ()
agentNonCombatMoveM = 
    do
        cellsInSight <- agentLookoutM
        pos <- environmentPositionM

        let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight
        let bestCells = selectBestCells bestMeasureSugarLevel pos unoccupiedCells

        (cellCoord, _) <- agentPickRandomM bestCells

        ifThenElse (null unoccupiedCells)
                    agentStayAndHarvestM
                    (agentMoveAndHarvestCellM cellCoord)

agentLookoutM :: State SugarScapeAgentOut [(EnvCoord, SugarScapeEnvCell)]
agentLookoutM = 
    do
        vis <- domainStateFieldM sugAgVision
        pos <- environmentPositionM 
        runEnvironmentM $ neighboursDistanceM pos vis

agentStayAndHarvestM :: State SugarScapeAgentOut ()
agentStayAndHarvestM = 
    do
        (cellCoord, _) <- agentCellOnPosM
        agentHarvestCellM cellCoord

agentMoveAndHarvestCellM :: EnvCoord -> State SugarScapeAgentOut ()
agentMoveAndHarvestCellM cellCoord = 
    do
        agentHarvestCellM cellCoord
        agentMoveToM cellCoord

agentMoveToM :: EnvCoord -> State SugarScapeAgentOut ()
agentMoveToM cellCoord = 
    do
        unoccupyPositionM

        s <- getDomainStateM
        aid <- agentIdM
        cell <- runEnvironmentM $ cellAtM cellCoord

        let cellOccupied = cell { sugEnvOccupier = Just (cellOccupier aid s) }

        runEnvironmentM $ changeCellAtM cellCoord cellOccupied
        changeEnvironmentPositionM cellCoord

agentHarvestCellM :: EnvCoord -> State SugarScapeAgentOut ()
agentHarvestCellM cellCoord = 
    do
        cell <- runEnvironmentM $ cellAtM cellCoord

        sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
        spiceLevelAgent <- domainStateFieldM sugAgSpiceLevel

        let sugarLevelCell = sugEnvSugarLevel cell
        let spiceLevelCell = sugEnvSpiceLevel cell

        let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
        let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

        updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

        let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
        runEnvironmentM $ changeCellAtM cellCoord cellHarvested
       
        -- NOTE: at the moment harvesting SPICE does not influence the polution
        let pol = sugarLevelCell * polutionHarvestFactor 
        agentPoluteCellM pol (cellCoord, cellHarvested)

agentAgeingM :: Double -> State SugarScapeAgentOut ()
agentAgeingM newAge =
    do
        updateDomainStateM (\s -> s { sugAgAge = newAge })

        whenM dieFromAgeM $ 
            do
                -- birthNewAgentM
                passWealthOnM
                agentDiesM

birthNewAgentM :: State SugarScapeAgentOut ()
birthNewAgentM = 
    do
        newAgentId <- agentIdM -- NOTE: we keep the old id
        newAgentCoord <- findUnoccpiedRandomPositionM
        newAgentDef <- runAgentRandomM $ randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviourM sugarScapeAgentConversationM
        createAgentM newAgentDef

    where
        findUnoccpiedRandomPositionM :: State SugarScapeAgentOut EnvCoord
        findUnoccpiedRandomPositionM =
            do
                env <- environmentM
                (c, coord) <- runAgentRandomM $ (randomCell env)

                ifThenElse (cellOccupied c) findUnoccpiedRandomPositionM (return coord)
                
dieFromAgeM :: State SugarScapeAgentOut Bool
dieFromAgeM = 
    do
        age <- domainStateFieldM sugAgAge
        maxAge <- domainStateFieldM sugAgMaxAge
        return $ age > maxAge

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSexM :: State SugarScapeAgentOut ()
agentSexM =
    do
        s <- getDomainStateM
        pos <- environmentPositionM
        env <- environmentM

        neighbourCells <- runEnvironmentM $ neighboursM pos
        nids <- neighbourIdsM 

        -- TODO: implement this as monadic
        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        let  nncsDupl = foldr (\(coord, _) acc -> (neighbours env coord) ++ acc) neighbourCells neighbourCells
        -- NOTE: the nncs are not unique, remove duplicates
        let nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
        let nncsUnoccupied = filter (isNothing . sugEnvOccupier . snd) nncsUnique

        when (isFertile s) (agentMatingConversationM nids nncsUnoccupied)

    where
        agentMatingConversationM :: [AgentId]
                                    -> [(EnvCoord, SugarScapeEnvCell)]
                                    -> State SugarScapeAgentOut ()
        agentMatingConversationM [] _ = conversationEndM
        agentMatingConversationM _ [] = conversationEndM
        agentMatingConversationM (receiverId:otherAis) allCoords@((coord, cell):cs) =
            do
                s <- getDomainStateM
                gender <- domainStateFieldM sugAgGender

                ifThenElse (satisfiesWealthForChildBearing s)
                    (conversationM (receiverId, (MatingRequest gender)) agentMatingConversationsReplyMonadicRunner)
                    conversationEndM
                
            where
                agentMatingConversationsReplyMonadicRunner :: SugarScapeAgentOut
                                                                -> Maybe (AgentMessage SugarScapeMsg)
                                                                -> SugarScapeAgentOut
                agentMatingConversationsReplyMonadicRunner ao mayReply = 
                    execState (agentMatingConversationsReplyM mayReply) ao

                agentMatingConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg) -> State SugarScapeAgentOut ()
                agentMatingConversationsReplyM Nothing = agentMatingConversationM otherAis allCoords  -- NOTE: the target was not found or does not have a handler, continue with the next
                agentMatingConversationsReplyM (Just (_, MatingReplyNo)) = agentMatingConversationM otherAis allCoords
                agentMatingConversationsReplyM (Just (senderId, (MatingReplyYes otherTup))) = 
                    do
                        initialSugarEndow <- domainStateFieldM sugAgSugarInit
                        aid <- agentIdM 

                        let mySugarContribution = initialSugarEndow / 2.0
                        mySugarMetab <- domainStateFieldM sugAgSugarMetab
                        mySpiceMetab <- domainStateFieldM sugAgSpiceMetab
                        myVision <- domainStateFieldM sugAgVision
                        myCulturalTag <- domainStateFieldM sugAgCulturalTag
                        myImmuneSysBorn <- domainStateFieldM sugAgImmuneSysBorn

                        let newBornId = senderId * aid   -- TODO: this is a real problem: which ids do we give our newborns?

                        newBornDef <- runAgentRandomM
                            (createNewBorn 
                                (newBornId, coord)
                                (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn)
                                otherTup
                                sugarScapeAgentBehaviourM
                                sugarScapeAgentConversationM)

                        let cell' = cell { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef))}
                        runEnvironmentM $ changeCellAtM coord cell'

                        updateDomainStateM (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) - mySugarContribution,
                                                      sugAgChildren = newBornId : (sugAgChildren s)})
                        createAgentM newBornDef

                        conversationM 
                            (receiverId, (MatingChild newBornId))
                            (\ao _ -> execState (agentMatingConversationM otherAis cs) ao)
                  
                agentMatingConversationsReplyM (Just (_, _)) = agentMatingConversationM otherAis allCoords  -- NOTE: unexpected/MatingChildAck reply, continue with the next

inheritSugarM :: SugarScapeAgentIn 
                    -> State SugarScapeAgentOut ()
inheritSugarM ain = onMessageM ain inheritSugarActionM
    where
        inheritSugarActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        inheritSugarActionM (_, (InheritSugar sug)) = updateDomainStateM (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + sug})
        inheritSugarActionM _ = return ()

-- TODO: implement handleMatingConversation as monadic ?
handleMatingConversationM :: (SugarScapeAgentGender)
                                -> SugarScapeAgentIn
                                -> (SugarScapeMsg, SugarScapeAgentIn)
handleMatingConversationM otherGender ain 
    | isFertile s &&
        satisfiesWealthForChildBearing s &&
        differentGender = (MatingReplyYes (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn), ain')
    | otherwise = (MatingReplyNo, ain)
    where
        s = aiState ain
        myGender = sugAgGender s
        differentGender = myGender /= otherGender

        -- NOTE: to be fertile an agent must have at least as much sugar as initially endowed, therefore it cannot go negative
        initialSugarEndow = sugAgSugarInit s
        sugarLevel = sugAgSugarLevel s
        mySugarContribution = initialSugarEndow / 2.0
        mySugarMetab = sugAgSugarMetab s
        mySpiceMetab = sugAgSpiceMetab s
        myVision = sugAgVision s
        myCulturalTag = sugAgCulturalTag s
        myImmuneSysBorn = sugAgImmuneSysBorn s

        s' = s { sugAgSugarLevel = sugarLevel - mySugarContribution }
        ain' = ain { aiState = s'}

agentCultureContactM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCultureContactM ain = 
    do
        onMessageM ain cultureContactActionM

        nids <- neighbourIdsM
        culturalTag <- domainStateFieldM sugAgCulturalTag

        broadcastMessageM (CulturalContact culturalTag) nids 

    where
        cultureContactActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        cultureContactActionM (_, (CulturalContact tagActive)) = 
            do
                agentTag <- domainStateFieldM sugAgCulturalTag
                agentTag' <- runAgentRandomM (cultureContact tagActive agentTag)
                
                let tribe = calculateTribe agentTag'

                updateDomainStateM (\s -> s { sugAgCulturalTag = agentTag',
                                              sugAgTribe = tribe})
        cultureContactActionM _ = return ()

agentKilledInCombatM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentKilledInCombatM ain = onMessageM ain killedInCombatActionM
    where
        killedInCombatActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        killedInCombatActionM (_, KilledInCombat) = killM -- NOTE: don't unoccupie position (as in agentdies) because it is occupied by the killer already
        killedInCombatActionM _ = return ()

agentCombatMoveM :: State SugarScapeAgentOut ()
agentCombatMoveM =
    do
        cellsInSight <- agentLookoutM
        myTribe <- domainStateFieldM sugAgTribe
        myWealth <- domainStateFieldM sugAgSugarLevel 

        let targetCells = filter (filterTargetCell (occupierCombatable myWealth myTribe)) cellsInSight
        
        ifThenElse (null targetCells)
            agentStayAndHarvestM
            $ do
                agentPos <- environmentPositionM

                -- TODO: refactor this into common function (searching for the best)
                let targeCellsWithPayoff = map cellPayoff targetCells

                let cellsSortedByPayoff = sortBy (\c1 c2 -> compare (snd c2) (snd c1)) targeCellsWithPayoff
                let bestCellPayoff = snd $ head cellsSortedByPayoff
                let bestCells = filter ((==bestCellPayoff) . snd) cellsSortedByPayoff

                let shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattan agentPos (fst . fst $ c1)) (distanceManhattan agentPos (fst . fst $ c2))) bestCells
                let shortestdistanceManhattan = distanceManhattan agentPos (fst . fst $ head shortestdistanceManhattanBestCells)
                let bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattan agentPos) . fst . fst) shortestdistanceManhattanBestCells

                bestCell@((_,_), payoff) <- agentPickRandomM bestShortestdistanceManhattanCells
                
                ifThenElseM (vulnerableToRetaliationM payoff)
                    agentStayAndHarvestM
                    (moveAndHarvestBestCellM bestCell)

    where
        -- NOTE: calculate if retalion is possible: is there an agent of the other tribe in my vision which is wealthier AFTER i have preyed on the current one?
        -- TODO: this is not very well specified in the SugarScape book. we don't know the vision of the other agent, and it is information we should not have access to
        vulnerableToRetaliationM :: Double -> State SugarScapeAgentOut Bool
        vulnerableToRetaliationM payoff =
            do
                sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
                cellsInSight <- agentLookoutM
                myTribe <- domainStateFieldM sugAgTribe

                let futureSugarLevel = (payoff + sugarLevelAgent)
                let retaliatingCells = filter (filterTargetCell (occupierRetaliator futureSugarLevel myTribe)) cellsInSight

                return $ (not . null) retaliatingCells
              
        moveAndHarvestBestCellM :: ((EnvCoord, SugarScapeEnvCell), Double) -> State SugarScapeAgentOut ()
        moveAndHarvestBestCellM ((cellCoord, cell), payoff) =
            do
                sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
                let newSugarLevelAgent = (payoff + sugarLevelAgent)

                unoccupyPositionM
                updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent })

                aid <- agentIdM
                s <- getDomainStateM

                let cellHarvestedAndOccupied = cell {
                        sugEnvSugarLevel = 0.0,
                        sugEnvOccupier = Just (cellOccupier aid s),
                        sugEnvPolutionLevel = 0
                }
                        
                runEnvironmentM $ changeCellAtM cellCoord cellHarvestedAndOccupied
                changeEnvironmentPositionM cellCoord

                when (cellOccupied cell) (killOccupierOfCellM cell)

        killOccupierOfCellM :: SugarScapeEnvCell -> State SugarScapeAgentOut ()
        killOccupierOfCellM cell = sendMessageM (occupierId, KilledInCombat)
            where
                occupier = fromJust $ sugEnvOccupier cell
                occupierId = sugEnvOccId occupier 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter IV: Sugar and Spice - Trade Comes to the Sugarscape
------------------------------------------------------------------------------------------------------------------------
agentTradingM :: SugarScapeAgentOut -> SugarScapeAgentOut
agentTradingM a = agentTradingConversation nids a
    where
        nids = neighbourIds a

        agentTradingConversation :: [AgentId]
                                    -> SugarScapeAgentOut
                                    -> SugarScapeAgentOut
        agentTradingConversation [] a = conversationEnd a
        agentTradingConversation (receiverId:otherAis) a = conversation a (receiverId, m) agentTradingConversationsReply
            where
                mrsSelf = agentMRS $ aoState a
                m = TradingOffer mrsSelf

                agentTradingConversationsReply :: SugarScapeAgentOut
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> SugarScapeAgentOut
                agentTradingConversationsReply a Nothing = agentTradingConversation otherAis a 
                agentTradingConversationsReply a (Just (_, TradingRefuse)) = agentTradingConversation otherAis a
                agentTradingConversationsReply a (Just (_, (TradingTransact _))) = agentTradingConversation otherAis a -- NOTE: other agent has transacted, continue with next
                agentTradingConversationsReply a (Just (senderId, (TradingAccept mrsOther))) 
                    | welfareIncreases = conversation aAfterTrade (senderId, TradingTransact mrsSelf) agentTradingConversationsReply
                    | otherwise = agentTradingConversation otherAis a 
                    where
                        welfareIncreases = agentTradeIncreaseWelfare s mrsOther
                        s = aoState a
                        s' = agentTradeExchange s mrsOther
                        aAfterTrade = a { aoState = s' }

-- NOTE: we ignore cross-over trades which is forbidden in the SugarScape-Book. We claim in our implementation it is not a problem as it works different.
--       also agents move on in the next step and won't be neighbours anyway, so a cross-over would not really become a problem in a way as Epstein and Axtell said it would create infinite recursion
--       which probably would occur in their oo-implementation because of direct method-calls
handleTradingOfferM :: Double
                        -> SugarScapeAgentIn
                        -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingOfferM mrsOther ain 
    | welfareIncreases = (TradingAccept mrsSelf, ain)     -- This makes the agent better off
    | otherwise = (TradingRefuse, ain)                      -- This trade would make the agent worse off, refuse the trade
    where
        s = aiState ain
        mrsSelf = agentMRS s
        welfareIncreases = agentTradeIncreaseWelfare s mrsOther

handleTradingTransactM :: Double
                            -> SugarScapeAgentIn
                            -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingTransactM mrsOther ain = (TradingTransact mrsOther, ainAfterTrade) -- NOTE: simply reply with the same transaction-message
    where
        s = aiState ain
        s' = agentTradeExchange s mrsOther
        ainAfterTrade = ain { aiState = s' }

agentCreditM :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCreditM ain a = agentRequestCreditM $ agentCheckCreditPaybackDueM $ agentCreditPaybackIncomingM ain $ agentCreditDeathIncomingM ain a

-- NOTE: for now only sugar is lended & borrowed, no spice
agentRequestCreditM :: SugarScapeAgentOut -> SugarScapeAgentOut
agentRequestCreditM a
    | hasNeighbours = agentCreditConversation nids a
    | otherwise = a
    where
        s = aoState a
        nids = neighbourIds a
        hasNeighbours = (not $ null nids)

        agentCreditConversation :: [AgentId]
                                    -> SugarScapeAgentOut
                                    -> SugarScapeAgentOut
        agentCreditConversation [] a = conversationEnd a
        agentCreditConversation (receiverId:otherAis) a 
            | isPotentialBorrower s = conversation a (receiverId, CreditRequest) agentCreditConversationsReply
            | otherwise = conversationEnd a
            where
                agentCreditConversationsReply :: SugarScapeAgentOut
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> SugarScapeAgentOut
                agentCreditConversationsReply a Nothing = agentCreditConversation otherAis a
                agentCreditConversationsReply a (Just (_, CreditRequestRefuse)) = agentCreditConversation otherAis a 
                agentCreditConversationsReply a (Just (lenderId, CreditOffer credit)) = agentCreditConversation otherAis aAfterBorrowing
                    where
                        s = aoState a
                        age = sugAgAge s

                        (faceValue, creditDuration, creditInterestRate) = credit
                        creditDueAge = age + creditDuration 

                        creditInfo = (lenderId, creditDueAge, credit)
                        aAfterBorrowing = updateDomainState a (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + faceValue,
                                                                    sugAgBorrowingCredits = creditInfo : sugAgBorrowingCredits s })

-- NOTE: if a borrower dies: notify the lenders so they know they take a loss (remove it from open credits)
-- NOTE: if a lender dies: notify the borrowers so they know they don't have to pay back
-- NOTE that we don't implement the inheritance-rule for loans
agentDeathHandleCreditsM :: SugarScapeAgentOut -> SugarScapeAgentOut
agentDeathHandleCreditsM a = aNotifiedBorrowers
    where
        s = aoState a
        lenderIds = map (\(lid, _, _) -> lid) (sugAgBorrowingCredits s)
        borrowerIds = sugAgLendingCredits s
        
        aNotifiedLenders = broadcastMessage a CreditBorrowerDied lenderIds
        aNotifiedBorrowers = broadcastMessage aNotifiedLenders CreditLenderDied borrowerIds

agentCreditDeathIncomingM :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCreditDeathIncomingM ain a = onMessage ain creditDeathAction a
    where
        creditDeathAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        creditDeathAction a (borrowerId, CreditBorrowerDied) = borrowerDied a borrowerId
        creditDeathAction a (lenderId, CreditLenderDied) = lenderDied a lenderId
        creditDeathAction a _ = a

        -- NOTE: the borrower could have borrowed multiple times from this lender, remove ALL ids
        borrowerDied :: SugarScapeAgentOut -> AgentId -> SugarScapeAgentOut
        borrowerDied a borrowerId = a' 
            where
                s = aoState a
                borrowers = sugAgLendingCredits s
                borrowersRemoved = filter (/=borrowerId) borrowers
                a' = updateDomainState a (\s -> s { sugAgLendingCredits = borrowersRemoved } )

        -- NOTE: the lender could have lended multiple times to this borrower, remove ALL credits
        lenderDied :: SugarScapeAgentOut -> AgentId -> SugarScapeAgentOut
        lenderDied a lenderId = a' 
            where
                s = aoState a
                borrowedCredits = sugAgBorrowingCredits s
                borrowersRemoved = filter (\(lId, _, _) -> lId /= lenderId) borrowedCredits
                a' = updateDomainState a (\s -> s { sugAgBorrowingCredits = borrowersRemoved } )

agentCreditPaybackIncomingM :: SugarScapeAgentIn -> SugarScapeAgentOut -> SugarScapeAgentOut
agentCreditPaybackIncomingM ain a = onMessage ain creditPaybackAction a
    where
        creditPaybackAction :: SugarScapeAgentOut -> AgentMessage SugarScapeMsg -> SugarScapeAgentOut
        creditPaybackAction a (_, (CreditPaybackHalf amount)) = halfCreditPayback a amount
        creditPaybackAction a (borrowerId, (CreditPaybackFull amount)) = fullCreditPayback a borrowerId amount
        creditPaybackAction a _ = a

        -- NOTE: in this case we don't remove the borrower because it has not yet payed back the whole credit
        halfCreditPayback :: SugarScapeAgentOut -> Double -> SugarScapeAgentOut
        halfCreditPayback = agentChangeSugarWealthM 

        -- NOTE: in this case we just remove the first borrower-id we find. It is possible that this lender has lended multiple times to the borrower but this doesnt matter in this case
        fullCreditPayback :: SugarScapeAgentOut -> AgentId -> Double -> SugarScapeAgentOut
        fullCreditPayback a borrowerId amount = agentChangeSugarWealthM a' amount
            where
                s = aoState a
                borrowers = sugAgLendingCredits s
                borrowersFirstRemoved = delete borrowerId borrowers
                a' = updateDomainState a (\s -> s { sugAgLendingCredits = borrowersFirstRemoved } )

agentChangeSugarWealthM :: SugarScapeAgentOut -> Double -> SugarScapeAgentOut
agentChangeSugarWealthM a amount = updateDomainState a (\s -> s { sugAgSugarLevel = newWealth } )
    where
        s = aoState a
        wealth = sugAgSugarLevel s 
        newWealth = wealth + amount

agentCheckCreditPaybackDueM :: SugarScapeAgentOut -> SugarScapeAgentOut
agentCheckCreditPaybackDueM a = aAfterPayback
    where
        s = aoState a
        borrowingCredits = sugAgBorrowingCredits s 

        (a', borrowingCredits') = foldr agentCheckCreditPaybackAux (a, []) borrowingCredits
        aAfterPayback = updateDomainState a' (\s -> s { sugAgBorrowingCredits = borrowingCredits'})

        agentCheckCreditPaybackAux :: SugarScapeCreditInfo -> (SugarScapeAgentOut, [SugarScapeCreditInfo]) -> (SugarScapeAgentOut, [SugarScapeCreditInfo])
        agentCheckCreditPaybackAux creditInfo@(lenderId, ageDue, credit) (a, accCredits) 
            | creditDue = (a, accCredits)
            | otherwise = (a, creditInfo : accCredits)
            where
                s = aoState a
                age = sugAgAge s
                creditDue = ageDue >= age

                paybackCredit :: (SugarScapeAgentOut, [SugarScapeCreditInfo])
                paybackCredit 
                    | fullPaybackPossible = (aAfterPayback, accCredits)
                    | otherwise = (aAfterPayback, newCreditInfo : accCredits)
                    where
                        (faceValue, creditDuration, creditInterestRate) = credit

                        wealth = sugAgSugarLevel s
                        dueAmount = faceValue + (faceValue * (creditInterestRate / 100))
                        fullPaybackPossible = wealth >= paybackAmount

                        paybackAmount = if fullPaybackPossible then paybackAmount else wealth * 0.5
                        paybackMessage = if fullPaybackPossible then (CreditPaybackFull paybackAmount) else (CreditPaybackHalf paybackAmount)

                        newCredit = (faceValue - paybackAmount, creditDuration, creditInterestRate)
                        newCreditInfo = (lenderId, age + creditDuration, newCredit)

                        aReducedWealth = agentChangeSugarWealthM a paybackAmount
                        aAfterPayback = sendMessage aReducedWealth (lenderId, paybackMessage)

handleCreditRequestM :: SugarScapeAgentIn -> AgentId -> (SugarScapeMsg, SugarScapeAgentIn)
handleCreditRequestM ain borrowerId
    | isLender = (CreditOffer credit, ainAfterCreditOffer)
    | otherwise = (CreditRequestRefuse, ain)
    where
        mayFaceValue = potentialLender s
        isLender = isJust mayFaceValue
        
        faceValue = fromJust mayFaceValue
        credit = (faceValue, lendingCreditDuration, lendingCreditInterestRate)

        s = aiState ain
        s' = s { sugAgSugarLevel = (sugAgSugarLevel s) - faceValue,
                sugAgLendingCredits = borrowerId : (sugAgLendingCredits s) }
        ainAfterCreditOffer = ain { aiState = s' }
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- Chapter V: Disease Processes
------------------------------------------------------------------------------------------------------------------------
agentDiseaseContactM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentDiseaseContactM ain = onMessageM ain diseaseContactActionM
    where
        diseaseContactActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        diseaseContactActionM (_, (DiseaseContact d)) = updateDomainStateM (\s -> s { sugAgDiseases = d : (sugAgDiseases s) } )
        diseaseContactActionM _ = return ()

agentDiseasesTransmitM :: State SugarScapeAgentOut ()
agentDiseasesTransmitM =
    do
        nids <- neighbourIdsM
        diseases <- domainStateFieldM sugAgDiseases
        s <- getDomainStateM

        let neighbourCount = length nids
        randDisease <- agentPickRandomMultipleM diseases neighbourCount

        let msgs = map (\(receiverId, disease) -> (receiverId, DiseaseContact disease)) (zip nids randDisease)
        let hasNeighbours = (not . null) nids

        when ((isDiseased s) && hasNeighbours) (sendMessagesM msgs)

agentImmunizeM :: State SugarScapeAgentOut ()
agentImmunizeM =
    do
        immuneSystem <- domainStateFieldM sugAgImmuneSys
        diseases <- domainStateFieldM sugAgDiseases

        let (immuneSystem', diseases') = foldr agentImmunizeAux (immuneSystem, []) diseases

        updateDomainStateM (\s -> s { sugAgImmuneSys = immuneSystem',
                                      sugAgDiseases = diseases' })

agentDiseaseProcessesM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentDiseaseProcessesM ain = agentDiseaseContactM ain >> agentDiseasesTransmitM >> agentImmunizeM
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CONVERSATION-HANDLER
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentConversationM :: SugarScapeAgentConversation
sugarScapeAgentConversationM ain (_, (MatingRequest tup)) = Just $ handleMatingConversationM tup ain
sugarScapeAgentConversationM ain (_, (MatingChild childId)) = Just (MatingChildAck, ain')
    where
        s = aiState ain
        s' = s { sugAgChildren = childId : (sugAgChildren s)}
        ain' = ain { aiState = s' }

sugarScapeAgentConversationM ain (_, (TradingOffer mrs)) = Just $ handleTradingOfferM mrs ain
sugarScapeAgentConversationM ain (_, (TradingTransact mrs)) = Just $ handleTradingTransactM mrs ain

sugarScapeAgentConversationM ain (borrowerId, CreditRequest) = Just $ handleCreditRequestM ain borrowerId

sugarScapeAgentConversationM _ _ = Nothing
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- BEHAVIOUR-CONTROL
------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviourFuncM :: Double -> SugarScapeAgentIn -> State SugarScapeAgentOut ()
sugarScapeAgentBehaviourFuncM age ain = return ()
{-
    do     
        let a0 = agentKilledInCombat ain a 
        if isDead a0 then
            agentDeathHandleCredits a0
            else
                do
                    let a1 = agentAgeing age a0
                    if isDead a1 then
                        agentDeathHandleCredits a1
                        else
                            do
                                let a2 = agentMetabolism a1
                                if isDead a2 then
                                    agentDeathHandleCredits a2
                                    else 
                                        do
                                            let a3 = agentNonCombatMove a2
                                            let a4 = inheritSugar ain a3
                                            -- let a5 = agentCultureContact ain a4
                                            -- let a6 = agentSex a5
                                            -- let a7 = agentTrading a5
                                            let a8 = agentCredit ain a4
                                            --let a9 = agentDiseaseProcesses ain a4
                                            a8
-}
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviourM :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviourM = proc ain ->
    do
        age <- time -< 0

        let ao = agentOutFromIn ain
        let ao' = execState (sugarScapeAgentBehaviourFuncM age ain) ao
        
        returnA -< ao'
------------------------------------------------------------------------------------------------------------------------