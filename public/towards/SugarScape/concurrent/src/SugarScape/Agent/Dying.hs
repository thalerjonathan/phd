{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Dying 
  ( agentDies
  , handleInheritance
  ) where

import Control.Monad.Random

import SugarScape.Agent.Common
import SugarScape.Agent.Loan
import SugarScape.Agent.Interface
import SugarScape.Core.Common 
import SugarScape.Core.Discrete
import SugarScape.Core.Model
import SugarScape.Core.Scenario
import SugarScape.Core.Utils

-- Here rule R is implemented, see page 32/33 "when an agent dies it is replaced by an agent 
-- of agent 0 having random genetic attributes, random position on the sugarscape..."
-- Also rule I is implemented, see page 67...
-- => will happen if agent starves to death (spice or sugar) or dies from age
agentDies :: RandomGen g
          => SugarScapeScenario
          -> SugarScapeAgent g
          -> AgentLocalMonad g (SugAgentOut g)
agentDies params asf = do
  unoccupyPosition
  ao  <- kill <$> agentObservableM
  ao' <- birthNewAgent params asf ao
  inheritance params ao'

birthNewAgent :: RandomGen g
              => SugarScapeScenario
              -> SugarScapeAgent g
              -> SugAgentOut g
              -> AgentLocalMonad g (SugAgentOut g)
birthNewAgent params asf ao
  | not $ spReplaceAgents params = return ao
  | otherwise = do
    newAid              <- nextAgentId
    myTribe             <- agentProperty sugAgTribe
    (newCoord, newCell) <- findUnoccpiedRandomPosition
    (newA, newAState)   <- randLift $ randomAgent params (newAid, newCoord) asf 
                              (\as -> case myTribe of
                                        Red  -> changeToRedTribe params as
                                        Blue -> changeToBlueTribe params as)

    -- need to occupy the cell to prevent other agents occupying it
    let occ      = occupier newAid newAState
        newCell' = newCell { sugEnvSiteOccupier = Just occ }
        
    envRun $ changeCellAt newCoord newCell' 

    return $ newAgent newA ao
  where
    -- TODO: the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => AgentLocalMonad g (Discrete2dCoord, SugEnvSite)
    findUnoccpiedRandomPosition = do
      (coord, c) <- randomCell
      ifThenElse
        (siteOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))

    randomCell :: RandomGen g => AgentLocalMonad g (Discrete2dCoord, SugEnvSite)
    randomCell = do
      (maxX, maxY) <- envRun dimensionsDisc2d

      randX <- randLift $ getRandomR (0, maxX - 1) 
      randY <- randLift $ getRandomR (0, maxY - 1)

      let randCoord = (randX, randY)
          
      randCell <- envRun $ cellAt randCoord
      return (randCoord, randCell)

inheritance :: RandomGen g
            => SugarScapeScenario
            -> SugAgentOut g
            -> AgentLocalMonad g (SugAgentOut g)
inheritance params ao0
    | not $ spInheritance params = return ao0
    | otherwise = do
      sugLvl   <- agentProperty sugAgSugarLevel
      children <- agentProperty sugAgChildren

      ls <- agentProperty sugAgLent
      -- children inherit all loans
      inheritLoans ls children
      -- notify borrowers that lender has died and will be inherited by children
      notifyBorrowers ls children

      -- only inherit in case 
      -- 1. there is sugar left (performance optimisation) (sugLvl is 0 in case the agent starved to death)
      -- 2. there are actually children
      if sugLvl > 0 && not (null children)
        then do
          let share = sugLvl / fromIntegral (length children)
          broadcastEvent children (Inherit share)
          agentObservableM
        else agentObservableM
  where
    inheritLoans :: RandomGen g 
                 => [Loan]
                 -> [AgentId] 
                 -> AgentLocalMonad g ()
    inheritLoans ls children 
        = mapM_ inheritLoan ls
      where
        inheritLoan :: Loan
                    -> AgentLocalMonad g ()
        inheritLoan l = sendEvents loanMsgs
          where
            newLoans = splitLoanLent (length children) l
            loanMsgs = map (\(c, l') -> (c, LoanInherit l')) (zip children newLoans)

    notifyBorrowers :: RandomGen g 
                    => [Loan]
                    -> [AgentId] 
                    -> AgentLocalMonad g ()
    notifyBorrowers ls children
        = broadcastEvent borrowerIds (LoanLenderDied children)
      where
        borrowerIds = map loanAgentId ls

        loanAgentId :: Loan -> AgentId
        loanAgentId (Loan _ aid _ _ ) = aid

handleInheritance :: RandomGen g
                  => Double
                  -> AgentLocalMonad g (SugAgentOut g)
handleInheritance share = do
  updateAgentState (\s -> s { sugAgSugarLevel = sugAgSugarLevel s + share })
  -- NOTE: need to update occupier-info in environment because wealth has (and MRS) changed
  updateSiteOccupied
  agentObservableM