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

agentCoordToPatchCoord :: ZombiesEnvironment -> Continuous2DCoord -> Discrete2dCoord
agentCoordToPatchCoord (as, ap, _) cc = cont2dTransDisc2d ap as cc

humanBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment
humanBehaviourM e@(as, ap, an) _ ain = 
    do
        updateDomainStateM (\s -> s { zAgentRole = Human })
        coord <- domainStateFieldM zAgentCoord
        let coordPatch = agentCoordToPatchCoord e coord

        let ns = neighbours coordPatch True ap
        let sortedNs = sortBy sortPatchesByZombies ns
        let (_, (_, maxZombiesCount)) = last sortedNs
        
        ifThenElse
            (maxZombiesCount > 0) -- read: any zombies within neighbourhood
            (do
                let (fewestZombiesPatch, _) = head sortedNs

                energy <- domainStateFieldM zHumanEnergyLevel
                ifThenElse
                    (energy > 0)
                    (flee coordPatch fewestZombiesPatch e)
                    (resetEnergy >> return e))
            (return e)
    where
        aid = aiId ain

        resetEnergy :: State ZombiesAgentOut ()
        resetEnergy = updateDomainStateM (\s -> s { zHumanEnergyLevel = zHumanEnergyInit s })

        reduceEnergy :: State ZombiesAgentOut ()
        reduceEnergy = updateDomainStateM (\s -> s { zHumanEnergyLevel = zHumanEnergyLevel s - 1 })

        flee :: Discrete2dCoord -> Discrete2dCoord -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        flee coordPatch targetPatch e@(as, ap, an)
            | coordPatch == targetPatch = return e
            | otherwise =
                do
                    coord <- domainStateFieldM zAgentCoord
                    let coord' = stepTo as humanSpeed coord (disc2dToCont2d targetPatch)
                    updateDomainStateM (\s -> s { zAgentCoord = coord' })
                    reduceEnergy

                    let ap0 = updateCellAt coordPatch (removeHuman aid) ap
                    let ap1 = updateCellAt targetPatch (addHuman aid) ap0
                    return (as, ap1, an)

zombieBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment
zombieBehaviourM e@(as, ap, an) _ ain = 
    do
        updateDomainStateM (\s -> s { zAgentRole = Zombie })
        coord <- domainStateFieldM zAgentCoord
        let coordPatch = agentCoordToPatchCoord e coord

        let ns = neighbours coordPatch True ap
        let sortedNs = sortBy sortPatchesByHumans ns
        let patch@(maxHumanCoord, (hs, _)) = last sortedNs

        e' <- moveTowards coordPatch maxHumanCoord e
        infect patch e'

    where
        moveTowards :: Discrete2dCoord -> Discrete2dCoord -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        moveTowards coordPatch targetPatch e 
            | coordPatch == targetPatch = return e
            | otherwise =
                do
                    coord <- domainStateFieldM zAgentCoord
                    let coord' = stepTo as zombieSpeed coord (disc2dToCont2d targetPatch)
                    updateDomainStateM (\s -> s { zAgentCoord = coord' })

                    let ap0 = updateCellAt coordPatch decZombie ap
                    let ap1 = updateCellAt targetPatch incZombie ap0
                    return (as, ap1, an)

        infect :: Discrete2dCell ZombiesPatch -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        infect (_, ([], _)) e = return e
        infect (coord, (hs, _)) e@(as, ap, an) =
            do
                h <- agentRandomPickM hs
                trace ("infecting " ++ (show h)) sendMessageToM h Infect

                let ap0 = updateCellAt coord (removeHuman h) ap
                let ap1 = updateCellAt coord incZombie ap0

                return (as, ap1, an)

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