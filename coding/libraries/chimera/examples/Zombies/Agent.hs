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

agentCoordToPatchCoord :: ZombiesEnvironment -> State ZombiesAgentOut Discrete2dCoord
agentCoordToPatchCoord (as, ap, _) = agentStateFieldM zAgentCoord >>= \coord -> return $ cont2dTransDisc2d ap as coord

humanBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment
humanBehaviourM e@(as, ap, an) _ ain = 
    do
        updateAgentStateM (\s -> s { zAgentRole = Human })
        originPatch <- agentCoordToPatchCoord e

        let ns = neighbours originPatch True ap
        ns' <- agentRandomShuffleM ns
        let sortedNs = sortBy sortPatchesByZombies ns'
        let (_, (_, maxZombiesCount)) = last sortedNs
        
        ifThenElse
            (maxZombiesCount > 0) -- read: any zombies within neighbourhood
            (do
                let fewestZombiesPatch = fst . head $ sortedNs

                energy <- agentStateFieldM zHumanEnergyLevel
                ifThenElse
                    (energy > 0)
                    (flee originPatch fewestZombiesPatch e)
                    (resetEnergy >> return e))
            (return e)
    where
        resetEnergy :: State ZombiesAgentOut ()
        resetEnergy = updateAgentStateM (\s -> s { zHumanEnergyLevel = zHumanEnergyInit s })

        reduceEnergy :: State ZombiesAgentOut ()
        reduceEnergy = updateAgentStateM (\s -> s { zHumanEnergyLevel = zHumanEnergyLevel s - 1 })

        flee :: Discrete2dCoord -> Discrete2dCoord -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        flee originPatch targetPatch e@(as, ap, an)
            | originPatch == targetPatch = return e
            | otherwise =
                do
                    coord <- agentStateFieldM zAgentCoord
                    let coord' = stepTo as humanSpeed coord (disc2dToCont2d targetPatch)
                    updateAgentStateM (\s -> s { zAgentCoord = coord' })
                    reduceEnergy

                    aid <- agentIdM
                    let ap0 = updateCellAt originPatch (removeHuman aid) ap
                    let ap1 = updateCellAt targetPatch (addHuman aid) ap0
                    return (as, ap1, an)

zombieBehaviourM :: ZombiesEnvironment 
                    -> Double 
                    -> ZombiesAgentIn 
                    -> State ZombiesAgentOut ZombiesEnvironment
zombieBehaviourM e@(as, ap, an) _ ain = 
    do
        updateAgentStateM (\s -> s { zAgentRole = Zombie })
        coord <- agentStateFieldM zAgentCoord
        originPatch <- agentCoordToPatchCoord e

        let ns = neighbours originPatch True ap
        ns' <- agentRandomShuffleM ns
        let sortedNs = sortBy sortPatchesByHumans ns'
        let patch@(maxHumanCoord, (hs, _)) = last sortedNs

        e' <- moveTowards originPatch maxHumanCoord e
        infect patch e'

    where
        moveTowards :: Discrete2dCoord -> Discrete2dCoord -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        moveTowards originPatch targetPatch e 
            | originPatch == targetPatch = return e
            | otherwise =
                do
                    coord <- agentStateFieldM zAgentCoord
                    let coord' = stepTo as zombieSpeed coord (disc2dToCont2d targetPatch)
                    updateAgentStateM (\s -> s { zAgentCoord = coord' })

                    let ap0 = updateCellAt originPatch decZombie ap
                    let ap1 = updateCellAt targetPatch incZombie ap0
                    return (as, ap1, an)

        infect :: Discrete2dCell ZombiesPatch -> ZombiesEnvironment -> State ZombiesAgentOut ZombiesEnvironment
        infect (_, ([], _)) e = return e -- no humans on this patch
        infect (coord, (hs, _)) e@(as, ap, an) =
            do
                h <- agentRandomPickM hs
                sendMessageToM h Infect

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