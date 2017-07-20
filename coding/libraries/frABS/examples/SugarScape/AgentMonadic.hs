module SugarScape.AgentMonadic (
    sugarScapeAgentConversationM,
    sugarScapeAgentBehaviourM
  ) where

import SugarScape.AgentCommon
import SugarScape.Model
import SugarScape.Environment

import FRP.FrABS

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
agentDiesM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentDiesM e = unoccupyPositionM e >>= (\e' -> killM >> return e')


unoccupyPositionM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
unoccupyPositionM e = 
    do
        coord <- domainStateFieldM sugAgCoord
        let cell = cellAt coord e
        let cellUnoccupied = cell { sugEnvOccupier = Nothing }
        return $ changeCellAt coord cellUnoccupied e

passWealthOnM :: State SugarScapeAgentOut ()
passWealthOnM =
    do
        sugarLevel <- domainStateFieldM sugAgSugarLevel
        childrenIds <- domainStateFieldM sugAgChildren

        let hasChildren = (not . null) childrenIds

        when hasChildren $
            do 
                let childrenCount = length childrenIds
                let childrenSugarShare = sugarLevel / fromIntegral childrenCount
                broadcastMessageM (InheritSugar childrenSugarShare) childrenIds

starvedToDeathM :: State SugarScapeAgentOut Bool
starvedToDeathM = 
    do
        sugar <- domainStateFieldM sugAgSugarLevel
        spice <- domainStateFieldM sugAgSpiceLevel
        return $ (sugar <= 0) || (spice <= 0)


agentMetabolismM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMetabolismM e =
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
        
        coord <- domainStateFieldM sugAgCoord
        let e' = poluteCell pol coord e

        ifThenElseM
            starvedToDeathM
            (agentDiesM e')
            (return e')

agentNonCombatMoveM :: SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentNonCombatMoveM e = 
    do
        cellsInSight <- agentLookoutM e
        coord <- domainStateFieldM sugAgCoord

        let unoccupiedCells = filter (cellUnoccupied . snd) cellsInSight
        let bestCells = selectBestCells bestMeasureSugarLevel coord unoccupiedCells

        (cellCoord, _) <- agentRandomPickM bestCells

        ifThenElse 
            (null unoccupiedCells)
            (agentHarvestCellM coord e)  -- NOTE: stay and harvest
            (agentMoveAndHarvestCellM cellCoord e)

agentLookoutM :: SugarScapeEnvironment -> State SugarScapeAgentOut [(Discrete2dCoord, SugarScapeEnvCell)]
agentLookoutM e = 
    do
        vis <- domainStateFieldM sugAgVision
        coord <- domainStateFieldM sugAgCoord
        return $ neighbourInDistance coord vis e

agentMoveAndHarvestCellM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMoveAndHarvestCellM cellCoord e = -- agentHarvestCellM cellCoord e >>= (\e' -> agentMoveToM cellCoord e')
    do
        e' <- agentHarvestCellM cellCoord e
        agentMoveToM cellCoord e'

agentMoveToM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentMoveToM cellCoord e = 
    do
        e' <- unoccupyPositionM e

        updateDomainStateM (\s -> s { sugAgCoord = cellCoord })

        s <- getDomainStateM
        aid <- agentIdM

        let cell = cellAt cellCoord e'
        let cellOccupied = cell { sugEnvOccupier = Just (cellOccupier aid s) }
        return $ changeCellAt cellCoord cellOccupied e'

agentHarvestCellM :: Discrete2dCoord -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentHarvestCellM cellCoord e = 
    do
        let cell = cellAt cellCoord e

        sugarLevelAgent <- domainStateFieldM sugAgSugarLevel
        spiceLevelAgent <- domainStateFieldM sugAgSpiceLevel

        let sugarLevelCell = sugEnvSugarLevel cell
        let spiceLevelCell = sugEnvSpiceLevel cell

        let newSugarLevelAgent = sugarLevelCell + sugarLevelAgent
        let newSpiceLevelAgent = spiceLevelCell + spiceLevelAgent

        updateDomainStateM (\s -> s { sugAgSugarLevel = newSugarLevelAgent, sugAgSpiceLevel = newSpiceLevelAgent })

        let cellHarvested = cell { sugEnvSugarLevel = 0.0, sugEnvSpiceLevel = 0.0 }
        let e' = changeCellAt cellCoord cellHarvested e
       
        -- NOTE: at the moment harvesting SPICE does not influence the polution
        let pol = sugarLevelCell * polutionHarvestFactor 
        return $ poluteCell pol cellCoord e'

agentAgeingM :: Double -> SugarScapeEnvironment -> State SugarScapeAgentOut SugarScapeEnvironment
agentAgeingM newAge e =
    do
        updateDomainStateM (\s -> s { sugAgAge = newAge })

        ifThenElseM
            dieFromAgeM
            (do
                birthNewAgentM e
                passWealthOnM
                agentDiesM e)
            (return e)

birthNewAgentM :: SugarScapeEnvironment -> State SugarScapeAgentOut ()
birthNewAgentM e = 
    do
        newAgentId <- agentIdM -- NOTE: we keep the old id
        newAgentCoord <- findUnoccpiedRandomPositionM
        newAgentDef <- agentRandomM $ randomAgent (newAgentId, newAgentCoord) sugarScapeAgentBehaviourM sugarScapeAgentConversationM
        createAgentM newAgentDef

    where
        findUnoccpiedRandomPositionM :: State SugarScapeAgentOut Discrete2dCoord
        findUnoccpiedRandomPositionM =
            do
                (c, coord) <- agentRandomM $ randomCell e
                ifThenElse
                    (cellOccupied c) 
                    findUnoccpiedRandomPositionM 
                    (return coord)
                
dieFromAgeM :: State SugarScapeAgentOut Bool
dieFromAgeM = 
    do
        age <- domainStateFieldM sugAgAge
        maxAge <- domainStateFieldM sugAgMaxAge
        return $ age > maxAge

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
agentSexM :: SugarScapeAgentIn -> SugarScapeEnvironment -> State SugarScapeAgentOut ()
agentSexM ain e =
    do
        s <- getDomainStateM
        coord <- domainStateFieldM sugAgCoord

        let neighbourCells = neighbours coord e
        nids <- neighbourIdsM e

        -- TODO: implement this as monadic
        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        let  nncsDupl = foldr (\(coord, _) acc -> neighbours coord e ++ acc) neighbourCells neighbourCells
        -- NOTE: the nncs are not unique, remove duplicates
        let nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
        let nncsUnoccupied = filter (isNothing . sugEnvOccupier . snd) nncsUnique

        when 
            (isFertile s) 
            (agentMatingConversationM nids nncsUnoccupied)

    where
        agentMatingConversationM :: [AgentId]
                                    -> [(Discrete2dCoord, SugarScapeEnvCell)]
                                    -> State SugarScapeAgentOut ()
        agentMatingConversationM [] _ = conversationEndM
        agentMatingConversationM _ [] = conversationEndM
        agentMatingConversationM (receiverId:otherAis) allCoords@((coord, cell):cs) =
            do
                s <- getDomainStateM
                gender <- domainStateFieldM sugAgGender

                ifThenElse 
                    (satisfiesWealthForChildBearing s)
                    (conversationM 
                        (receiverId, MatingRequest gender) 
                        (conversationReplyMonadicRunner agentMatingConversationsReplyM))
                    conversationEndM

            where
                agentMatingConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg) 
                                                    -> SugarScapeEnvironment
                                                    -> State SugarScapeAgentOut SugarScapeEnvironment
                agentMatingConversationsReplyM Nothing e = agentMatingConversationM otherAis allCoords >> return e  -- NOTE: the target was not found or does not have a handler, continue with the next
                agentMatingConversationsReplyM (Just (_, MatingReplyNo)) e = agentMatingConversationM otherAis allCoords >> return e
                agentMatingConversationsReplyM (Just (senderId, MatingReplyYes otherTup)) e = 
                    do
                        initialSugarEndow <- domainStateFieldM sugAgSugarInit
                        -- aid <- agentIdM 

                        let mySugarContribution = initialSugarEndow / 2.0
                        mySugarMetab <- domainStateFieldM sugAgSugarMetab
                        mySpiceMetab <- domainStateFieldM sugAgSpiceMetab
                        myVision <- domainStateFieldM sugAgVision
                        myCulturalTag <- domainStateFieldM sugAgCulturalTag
                        myImmuneSysBorn <- domainStateFieldM sugAgImmuneSysBorn

                        let newBornId = nextAgentId ain

                        newBornDef <- agentRandomM
                            (createNewBorn 
                                (newBornId, coord)
                                (mySugarContribution, mySugarMetab, mySpiceMetab, myVision, myCulturalTag, myImmuneSysBorn)
                                otherTup
                                sugarScapeAgentBehaviourM
                                sugarScapeAgentConversationM)

                        let cell' = cell { sugEnvOccupier = Just (cellOccupier newBornId (adState newBornDef))}
                        let e' = changeCellAt coord cell' e

                        updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s - mySugarContribution,
                                                      sugAgChildren = newBornId : sugAgChildren s})
                        
                        createAgentM newBornDef

                        conversationM 
                            (receiverId, MatingChild newBornId)
                            (conversationIgnoreReplyMonadicRunner (agentMatingConversationM otherAis cs))
                            --(\ao _ -> execState (agentMatingConversationM otherAis cs) ao)

                        return e'
                        
                agentMatingConversationsReplyM (Just (_, _)) e = agentMatingConversationM otherAis allCoords >> return e  -- NOTE: unexpected/MatingChildAck reply, continue with the next

inheritSugarM :: SugarScapeAgentIn 
                    -> State SugarScapeAgentOut ()
inheritSugarM ain = onMessageMState inheritSugarActionM ain
    where
        inheritSugarActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        inheritSugarActionM (_, InheritSugar sug) = updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + sug})
        inheritSugarActionM _ = return ()

-- TODO: implement handleMatingConversation as monadic ?
handleMatingConversationM :: SugarScapeAgentGender
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
        onMessageMState cultureContactActionM ain

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
agentKilledInCombatM ain = onMessageMState killedInCombatActionM ain
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
              
        moveAndHarvestBestCellM :: ((Discrete2dCoord, SugarScapeEnvCell), Double) -> State SugarScapeAgentOut ()
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
agentTradingM :: State SugarScapeAgentOut ()
agentTradingM = neighbourIdsM >>= agentTradingConversationM
    where
        agentTradingConversationM :: [AgentId]
                                    -> State SugarScapeAgentOut ()
        agentTradingConversationM [] = conversationEndM
        agentTradingConversationM (receiverId:otherAis) = 
            do
                s <- getDomainStateM
                let mrsSelf = agentMRS s
                conversationM 
                    (receiverId, (TradingOffer mrsSelf)) 
                    (conversationReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf)
            
            where
                agentTradingConversationsReplyM :: Double
                                                    -> Maybe (AgentMessage SugarScapeMsg)
                                                    -> State SugarScapeAgentOut ()
                agentTradingConversationsReplyM _ Nothing = agentTradingConversationM otherAis 
                agentTradingConversationsReplyM _ (Just (_, TradingRefuse)) = agentTradingConversationM otherAis 
                agentTradingConversationsReplyM _ (Just (_, (TradingTransact _))) = agentTradingConversationM otherAis  -- NOTE: other agent has transacted, continue with next
                agentTradingConversationsReplyM mrsSelf (Just (senderId, (TradingAccept mrsOther))) =
                    do
                        s <- getDomainStateM

                        let welfareIncreases = agentTradeIncreaseWelfare s mrsOther
                        
                        ifThenElse welfareIncreases
                            (do
                                let s' = agentTradeExchange s mrsOther
                                setDomainStateM s'
                                conversationM 
                                    (senderId, TradingTransact mrsSelf) 
                                    (conversationReplyMonadicRunner $ agentTradingConversationsReplyM mrsSelf))
                            (agentTradingConversationM otherAis)

agentCreditM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCreditM ain = agentRequestCreditM >> agentCheckCreditPaybackDueM >> agentCreditPaybackIncomingM ain >> agentCreditDeathIncomingM ain

-- NOTE: for now only sugar is lended & borrowed, no spice
agentRequestCreditM :: State SugarScapeAgentOut ()
agentRequestCreditM =
    do
        nids <- neighbourIdsM
        let hasNeighbours = (not $ null nids)
        when hasNeighbours (agentCreditConversationM nids)

    where
        agentCreditConversationM :: [AgentId]
                                    -> State SugarScapeAgentOut ()
        agentCreditConversationM [] = conversationEndM
        agentCreditConversationM (receiverId:otherAis) =
            do
                s <- getDomainStateM
                ifThenElse (isPotentialBorrower s)
                    (conversationM (receiverId, CreditRequest) 
                        (conversationReplyMonadicRunner agentCreditConversationsReplyM))
                    conversationEndM
            where
                agentCreditConversationsReplyM :: Maybe (AgentMessage SugarScapeMsg)
                                                    -> State SugarScapeAgentOut ()
                agentCreditConversationsReplyM Nothing = agentCreditConversationM otherAis
                agentCreditConversationsReplyM (Just (_, CreditRequestRefuse)) = agentCreditConversationM otherAis 
                agentCreditConversationsReplyM (Just (lenderId, CreditOffer credit)) = 
                    do
                        age <- domainStateFieldM sugAgAge

                        let (faceValue, creditDuration, creditInterestRate) = credit
                        let creditDueAge = age + creditDuration 
                        let creditInfo = (lenderId, creditDueAge, credit)
                        updateDomainStateM (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + faceValue,
                                                        sugAgBorrowingCredits = creditInfo : sugAgBorrowingCredits s })

                        agentCreditConversationM otherAis

-- NOTE: if a borrower dies: notify the lenders so they know they take a loss (remove it from open credits)
-- NOTE: if a lender dies: notify the borrowers so they know they don't have to pay back
-- NOTE that we don't implement the inheritance-rule for loans
agentDeathHandleCreditsM :: State SugarScapeAgentOut ()
agentDeathHandleCreditsM = 
    do
        borrowedCredits <- domainStateFieldM sugAgBorrowingCredits
        borrowerIds <- domainStateFieldM sugAgLendingCredits

        let lenderIds = map (\(lid, _, _) -> lid) borrowedCredits
        
        broadcastMessageM CreditBorrowerDied lenderIds
        broadcastMessageM CreditLenderDied borrowerIds

agentCreditDeathIncomingM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCreditDeathIncomingM ain = onMessageMState creditDeathActionM ain
    where
        creditDeathActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        creditDeathActionM (borrowerId, CreditBorrowerDied) = borrowerDiedM borrowerId
        creditDeathActionM (lenderId, CreditLenderDied) = lenderDiedM lenderId
        creditDeathActionM _ = return ()

        -- NOTE: the borrower could have borrowed multiple times from this lender, remove ALL ids
        borrowerDiedM :: AgentId -> State SugarScapeAgentOut ()
        borrowerDiedM borrowerId = 
            do
                borrowers <- domainStateFieldM sugAgLendingCredits
                let borrowersRemoved = filter (/=borrowerId) borrowers
                updateDomainStateM (\s -> s { sugAgLendingCredits = borrowersRemoved } )

        -- NOTE: the lender could have lended multiple times to this borrower, remove ALL credits
        lenderDiedM :: AgentId -> State SugarScapeAgentOut ()
        lenderDiedM lenderId = 
            do
                borrowedCredits <- domainStateFieldM sugAgBorrowingCredits
                let borrowersRemoved = filter (\(lId, _, _) -> lId /= lenderId) borrowedCredits
                updateDomainStateM (\s -> s { sugAgBorrowingCredits = borrowersRemoved } )

agentCreditPaybackIncomingM :: SugarScapeAgentIn -> State SugarScapeAgentOut ()
agentCreditPaybackIncomingM ain = onMessageMState creditPaybackActionM ain
    where
        creditPaybackActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        creditPaybackActionM (_, (CreditPaybackHalf amount)) = halfCreditPaybackM amount
        creditPaybackActionM (borrowerId, (CreditPaybackFull amount)) = fullCreditPaybackM borrowerId amount
        creditPaybackActionM _ = return ()

        -- NOTE: in this case we don't remove the borrower because it has not yet payed back the whole credit
        halfCreditPaybackM :: Double -> State SugarScapeAgentOut ()
        halfCreditPaybackM = agentChangeSugarWealthM 

        -- NOTE: in this case we just remove the first borrower-id we find. It is possible that this lender has lended multiple times to the borrower but this doesnt matter in this case
        fullCreditPaybackM :: AgentId -> Double -> State SugarScapeAgentOut ()
        fullCreditPaybackM borrowerId amount = 
            do
                borrowers <- domainStateFieldM sugAgLendingCredits
                let borrowersFirstRemoved = delete borrowerId borrowers
                updateDomainStateM (\s -> s { sugAgLendingCredits = borrowersFirstRemoved } )
                agentChangeSugarWealthM amount

agentChangeSugarWealthM :: Double -> State SugarScapeAgentOut ()
agentChangeSugarWealthM amount = updateDomainStateM (\s -> s { sugAgSugarLevel = (sugAgSugarLevel s) + amount } )

agentCheckCreditPaybackDueM :: State SugarScapeAgentOut ()
agentCheckCreditPaybackDueM = 
    do
        borrowedCredits <- domainStateFieldM sugAgBorrowingCredits

        borrowedCredits' <- foldM agentCheckCreditPaybackAuxM [] borrowedCredits
        updateDomainStateM (\s -> s { sugAgBorrowingCredits = borrowedCredits'})

    where
        agentCheckCreditPaybackAuxM :: [SugarScapeCreditInfo] 
                                        -> SugarScapeCreditInfo 
                                        -> State SugarScapeAgentOut [SugarScapeCreditInfo]
        agentCheckCreditPaybackAuxM accCredits creditInfo@(lenderId, ageDue, credit) =
            do
                age <- domainStateFieldM sugAgAge
                let creditDue = ageDue >= age

                ifThenElse creditDue
                    (paybackCredit age)
                    (return $ creditInfo : accCredits)

            where
                paybackCredit :: Double -> State SugarScapeAgentOut [SugarScapeCreditInfo]
                paybackCredit age =
                    do
                        wealth <- domainStateFieldM sugAgSugarLevel

                        let (faceValue, creditDuration, creditInterestRate) = credit
                        let dueAmount = faceValue + (faceValue * (creditInterestRate / 100))
                        let fullPaybackPossible = wealth >= dueAmount
                        
                        let paybackAmount = if fullPaybackPossible then dueAmount else wealth * 0.5
                        let paybackMessage = if fullPaybackPossible then (CreditPaybackFull paybackAmount) else (CreditPaybackHalf paybackAmount)

                        let newCredit = (faceValue - paybackAmount, creditDuration, creditInterestRate)
                        let newCreditInfo = (lenderId, age + creditDuration, newCredit)

                        agentChangeSugarWealthM paybackAmount
                        sendMessageM (lenderId, paybackMessage)

                        ifThenElse fullPaybackPossible
                            (return accCredits)
                            (return $ newCreditInfo : accCredits)

-- NOTE: we ignore cross-over trades which is forbidden in the SugarScape-Book. We claim in our implementation it is not a problem as it works different.
--       also agents move on in the next step and won't be neighbours anyway, so a cross-over would not really become a problem in a way as Epstein and Axtell said it would create infinite recursion
--       which probably would occur in their oo-implementation because of direct method-calls
-- TODO: monadic-refactoring
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

-- TODO: monadic-refactoring
handleTradingTransactM :: Double
                            -> SugarScapeAgentIn
                            -> (SugarScapeMsg, SugarScapeAgentIn)
handleTradingTransactM mrsOther ain = (TradingTransact mrsOther, ainAfterTrade) -- NOTE: simply reply with the same transaction-message
    where
        s = aiState ain
        s' = agentTradeExchange s mrsOther
        ainAfterTrade = ain { aiState = s' }

-- TODO: monadic-refactoring
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
agentDiseaseContactM ain = onMessageMState diseaseContactActionM ain
    where
        diseaseContactActionM :: AgentMessage SugarScapeMsg -> State SugarScapeAgentOut ()
        diseaseContactActionM (_, (DiseaseContact d)) = updateDomainStateM (\s -> s { sugAgDiseases = d : (sugAgDiseases s) } )
        diseaseContactActionM _ = return ()

agentDiseasesTransmitM :: State SugarScapeAgentOut ()
agentDiseasesTransmitM =
    do
        diseases <- domainStateFieldM sugAgDiseases

        when (not . null $ diseases)
            $ do
                nids <- neighbourIdsM
                let neighbourCount = length nids
                randDisease <- agentPickRandomMultipleM diseases neighbourCount

                let msgs = map (\(receiverId, disease) -> (receiverId, DiseaseContact disease)) (zip nids randDisease)
                let hasNeighbours = (not . null) nids

                when hasNeighbours $ sendMessagesM msgs

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
-- TODO: monadic-refactoring
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
sugarScapeAgentBehaviourFuncM :: SugarScapeEnvironment 
                                    -> Double 
                                    -> SugarScapeAgentIn 
                                    -> State SugarScapeAgentOut SugarScapeEnvironment
sugarScapeAgentBehaviourFuncM e age ain = 
    do     
        agentKilledInCombatM ain

        ifThenElseM isDeadM
            agentDeathHandleCreditsM
            $ do
                agentAgeingM age
                ifThenElseM isDeadM
                    agentDeathHandleCreditsM
                    $ do
                        agentMetabolismM
                        ifThenElseM isDeadM
                            agentDeathHandleCreditsM
                            $ do
                                agentNonCombatMoveM
                                inheritSugarM ain
                                agentCultureContactM  ain
                                agentSexM ain
                                agentTradingM
                                agentCreditM ain
                                agentDiseaseProcessesM ain
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
sugarScapeAgentBehaviourM :: SugarScapeAgentBehaviour
sugarScapeAgentBehaviourM = agentMonadic sugarScapeAgentBehaviourFuncM
------------------------------------------------------------------------------------------------------------------------