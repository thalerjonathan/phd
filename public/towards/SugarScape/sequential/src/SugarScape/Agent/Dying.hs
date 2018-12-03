{-# LANGUAGE FlexibleContexts #-}
module SugarScape.Agent.Dying 
  ( agentDies
  , handleInheritance
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

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
          => SugarScapeAgent g
          -> AgentLocalMonad g (SugAgentOut g)
agentDies asf = do
  unoccupyPosition
  ao  <- kill <$> agentObservableM
  ao' <- birthNewAgent asf ao
  inheritance ao'

birthNewAgent :: RandomGen g
              => SugarScapeAgent g
              -> SugAgentOut g
              -> AgentLocalMonad g (SugAgentOut g)
birthNewAgent asf ao =
 ifThenElseM
    ((not . spReplaceAgents) <$> scenario)
    (return ao)
    (do
      sc                  <- scenario
      newAid              <- absStateLift nextAgentId
      myTribe             <- agentProperty sugAgTribe
      (newCoord, newCell) <- findUnoccpiedRandomPosition
      (newA, newAState)   <- randLift $ randomAgent sc (newAid, newCoord) asf 
                                (\as -> case myTribe of
                                          Red  -> changeToRedTribe sc as
                                          Blue -> changeToBlueTribe sc as)

      -- need to occupy the cell to prevent other agents occupying it
      let occ      = occupier newAid newAState
          newCell' = newCell { sugEnvSiteOccupier = Just occ }
          
      envLift $ changeCellAtM newCoord newCell' 

      return $ newAgent newA ao)
  where
    -- TODO: the more cells occupied the less likely an unoccupied position will be found
    -- => restrict number of recursions and if not found then take up same position
    findUnoccpiedRandomPosition :: RandomGen g
                                => AgentLocalMonad g (Discrete2dCoord, SugEnvSite)
    findUnoccpiedRandomPosition = do
      e          <- envLift get
      (c, coord) <- randLift $ randomCell e -- TODO: replace by randomCellM
      ifThenElse
        (siteOccupied c) 
        findUnoccpiedRandomPosition
        (return (coord, c))

inheritance :: RandomGen g
            => SugAgentOut g
            -> AgentLocalMonad g (SugAgentOut g)
inheritance ao0 =
    ifThenElseM
      ((not . spInheritance) <$> scenario)
      (return ao0)
      (do
        sugLvl   <- agentProperty sugAgSugarLevel
        children <- agentProperty sugAgChildren

        ls <- agentProperty sugAgLent
        -- children inherit all loans
        let ao  = inheritLoans ls children ao0
        -- notify borrowers that lender has died and will be inherited by children
            ao' = notifyBorrowers ls children ao

        -- only inherit in case 
        -- 1. there is sugar left (performance optimisation) (sugLvl is 0 in case the agent starved to death)
        -- 2. there are actually children
        if sugLvl > 0 && not (null children)
          then do
            let share = sugLvl / fromIntegral (length children)
            return $ broadcastEvent children (Inherit share) ao'
          else return ao')
  where
    inheritLoans :: RandomGen g 
                 => [Loan]
                 -> [AgentId] 
                 -> SugAgentOut g
                 -> SugAgentOut g
    inheritLoans ls children ao
        = foldr inheritLoan ao ls
      where
        inheritLoan :: Loan
                    -> SugAgentOut g
                    -> SugAgentOut g
        inheritLoan l = sendEvents loanMsgs
          where
            newLoans = splitLoanLent (length children) l
            loanMsgs = map (\(c, l') -> (c, LoanInherit l')) (zip children newLoans)

    notifyBorrowers :: RandomGen g 
                    => [Loan]
                    -> [AgentId] 
                    -> SugAgentOut g
                    -> SugAgentOut g
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