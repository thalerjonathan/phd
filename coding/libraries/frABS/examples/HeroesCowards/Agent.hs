module HeroesCowards.Agent (
    heroesCowardsAgentBehaviour
  ) where

import HeroesCowards.Model

import FRP.FrABS

import Control.Monad
import Control.Monad.Trans.State

-------------------------------------------------------------------------------
-- Monadic implementation
-------------------------------------------------------------------------------
decidePosition :: Continuous2dCoord -> Continuous2dCoord -> HACRole -> Continuous2dCoord
decidePosition friendCoord enemyCoord role
    | Hero == role = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendVec = vecFromCoords friendCoord enemyCoord
        halfVec = multCoord 0.5 enemyFriendVec 
        coverPosition = addCoord friendCoord halfVec
        hidePosition = subCoord friendCoord halfVec

agentMove :: HACEnvironment -> HACRole -> State HACAgentOut ()
agentMove e role = 
    do
        coord <- agentStateFieldM hacCoord
        friendCoord <- agentStateFieldM hacFriendCoord
        enemyCoord <- agentStateFieldM hacEnemyCoord

        let target = decidePosition friendCoord enemyCoord role
        let targetDir = vecNorm $ vecFromCoords coord target

        let dt = 1.0 -- TODO: can we obtain it somehow?
        let stepWidth = stepWidthPerTimeUnit * dt
        let newPos = addCoord coord (multCoord stepWidth targetDir)
        let newPosWrapped = wrapCont2dEnv e newPos

        updateAgentStateM (\s -> s { hacCoord = newPosWrapped })

hacAgent :: HACRole -> HACAgentBehaviourReadEnv
hacAgent role e _ ain = 
    do
        onMessageMState handlePositionUpdate ain
        agentMove e role
        onMessageMState handlePositionRequest ain
        broadcastCurrentPosition

handlePositionUpdate :: AgentMessage HACMsg -> State HACAgentOut ()
handlePositionUpdate (senderId, PositionUpdate coord) =
    do
        friend <- agentStateFieldM hacFriend
        enemy <- agentStateFieldM hacEnemy
        when (senderId == friend) (updateAgentStateM (\s -> s { hacFriendCoord = coord }))
        when (senderId == enemy) (updateAgentStateM (\s -> s { hacEnemyCoord = coord }))
handlePositionUpdate _ = return ()

handlePositionRequest :: AgentMessage HACMsg -> State HACAgentOut () 
handlePositionRequest (senderId, PositionRequest) = 
    do
        coord <- agentStateFieldM hacCoord
        sendMessageM (senderId, PositionUpdate coord)
handlePositionRequest _ = return ()

broadcastCurrentPosition :: State HACAgentOut () 
broadcastCurrentPosition = 
    do
        friend <- agentStateFieldM hacFriend
        enemy <- agentStateFieldM hacEnemy
        broadcastMessageM PositionRequest [friend, enemy]

heroesCowardsAgentBehaviour :: HACRole -> HACAgentBehaviour
heroesCowardsAgentBehaviour role = agentMonadicReadEnv (hacAgent role)
-------------------------------------------------------------------------------