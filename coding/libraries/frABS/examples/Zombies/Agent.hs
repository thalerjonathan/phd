{-# LANGUAGE Arrows #-}
module Zombies.Agent (
	zombie,
	human
  ) where

import Zombies.Model

import FRP.FrABS

import FRP.Yampa
    
import Data.List
import Control.Monad.Trans.State
import Control.Monad.IfElse

import Debug.Trace

humanBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment     
humanBehaviourM e _ ain = 
    do
        let p = zAgentPatches e

        coord <- domainStateFieldM zAgentCoord

        let ns = neighbours (cont2dToDisc2d coord) p
        let sortedNs = sortBy (\(_, (_, z1)) (_, (_, z2)) -> compare z2 z1) ns
        let (fewestZombiesCoord, _) = head sortedNs
        let (_, (_, maxZombiesCount)) = last sortedNs

        let coord' = trace ("maxZombiesCount = " ++ (show maxZombiesCount)) (stepTo (zAgentSpace e) 0.01 coord (disc2dToCont2d fewestZombiesCoord))

        ifThenElse
            (maxZombiesCount > 0)
            (do
                let p0 = updateCellAt (cont2dToDisc2d coord) decHuman p
                let p1 = updateCellAt (cont2dToDisc2d coord') incHuman p0

                updateDomainStateM (\s -> s { zAgentCoord = coord' })

                return e { zAgentPatches = p1 })
            (return e)

zombieBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment     
zombieBehaviourM e _ ain = 
    do
        
        return e

------------------------------------------------------------------------------------------------------------------------
-- BEHAVIOURS
------------------------------------------------------------------------------------------------------------------------
human :: ZombiesAgentBehaviour
human = transitionOnMessage
                Infect
                (agentMonadic humanBehaviourM)
                (agentMonadic zombieBehaviourM)

zombie :: ZombiesAgentBehaviour
zombie = agentMonadic zombieBehaviourM
------------------------------------------------------------------------------------------------------------------------