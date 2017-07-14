module HeroesCowards.Agent (
    heroesCowardsAgentBehaviour
  ) where

import HeroesCowards.Model

import FRP.FrABS

import Control.Monad
import Control.Monad.Trans.State

-------------------------------------------------------------------------------
-- Geometric Utilities
-------------------------------------------------------------------------------
data HACWorldType = Infinite | Border | Wraping deriving (Eq, Show)

multPos :: ContPosition -> Double -> ContPosition
multPos (x, y) s = (x*s, y*s)

addPos :: ContPosition -> ContPosition -> ContPosition
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subPos :: ContPosition -> ContPosition -> ContPosition
subPos (x1, y1) (x2, y2) = (x1-x2, y1-y2)

posDir :: ContPosition -> ContPosition -> ContPosition
posDir (x1, y1) (x2, y2) = (x2-x1, y2-y1)

vecLen :: ContPosition -> Double
vecLen (x, y) = sqrt( x * x + y * y )

vecNorm :: ContPosition -> ContPosition
vecNorm (x, y)
    | len == 0 = (0, 0)
    | otherwise = (x / len, y / len)
    where
        len = vecLen (x, y)

clip :: ContPosition -> ContPosition
clip (x, y) = (clippedX, clippedY)
    where
        clippedX = max (-1.0) (min x 1.0)
        clippedY = max (-1.0) (min y 1.0)

wrap :: ContPosition -> ContPosition
wrap (x, y) = (wrappedX, wrappedY)
    where
        wrappedX = wrapValue x
        wrappedY = wrapValue y

wrapValue :: Double -> Double
wrapValue v
    | v > 1.0 = -1.0
    | v < -1.0 = 1.0
    | otherwise = v

worldTransform :: HACWorldType -> (ContPosition -> ContPosition)
worldTransform wt
    | wt == Border = clip
    | wt == Wraping = wrap
    | otherwise = id
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Monadic implementation
-------------------------------------------------------------------------------
decidePosition :: ContPosition -> ContPosition -> HACRole -> ContPosition
decidePosition friendPos enemyPos role
    | Hero == role = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendDir = posDir friendPos enemyPos
        halfPos = multPos enemyFriendDir 0.5
        coverPosition = addPos friendPos halfPos
        hidePosition = subPos friendPos halfPos

agentMove :: HACRole -> State HACAgentOut ()
agentMove role = 
    do
        pos <- domainStateFieldM hacPos
        friendPos <- domainStateFieldM hacFriendPos
        enemyPos <- domainStateFieldM hacEnemyPos

        let target = decidePosition friendPos enemyPos role
        let targetDir = vecNorm $ posDir pos target

        let dt = 1.0 -- TODO: can we obtain it somehow?
        let stepWidth = stepWidthPerTimeUnit * dt
        let newPos = worldTransform Border $ addPos pos (multPos targetDir stepWidth)

        updateDomainStateM (\s -> s { hacPos = newPos })

hacAgent :: HACRole -> Double -> HACAgentIn -> State HACAgentOut ()
hacAgent role _ ain = 
    do
        onMessageMState handlePositionUpdate ain
        agentMove role
        onMessageMState handlePositionRequest ain

        aids <- agentNeighbourNodesM
        broadcastMessageM PositionRequest aids

handlePositionUpdate :: AgentMessage HACMsg -> State HACAgentOut ()
handlePositionUpdate (senderId, PositionUpdate pos) =
    do
        friend <- domainStateFieldM hacFriend
        enemy <- domainStateFieldM hacEnemy
        when (senderId == friend) (updateDomainStateM (\s -> s { hacFriendPos = pos }))
        when (senderId == enemy) (updateDomainStateM (\s -> s { hacEnemyPos = pos }))
handlePositionUpdate _ = return ()


handlePositionRequest :: AgentMessage HACMsg -> State HACAgentOut () 
handlePositionRequest (senderId, PositionRequest) = 
    do
        pos <- domainStateFieldM hacPos
        sendMessageM (senderId, PositionUpdate pos)
handlePositionRequest _ = return ()

heroesCowardsAgentBehaviour :: HACRole -> HACAgentBehaviour
heroesCowardsAgentBehaviour role = agentMonadic (hacAgent role)
-------------------------------------------------------------------------------