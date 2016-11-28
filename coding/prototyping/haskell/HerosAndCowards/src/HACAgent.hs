module HACAgent (
    AgentId,
    AgentPosition,
    AgentState (..),
    AgentIn (..),
    AgentOut (..),
    cover,
    hide,
    decidePosition,
    createAgents
  ) where

import System.Random
import Control.DeepSeq

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------

type AgentId = Int
type AgentPosition = (Double, Double)

data AgentState = AgentState {
    agentPos :: AgentPosition,
    enemy :: AgentId,
    friend :: AgentId,
    hero :: Bool
}

data AgentIn = AgentIn {
    agentInAgents :: [AgentPosition]
}

data AgentOut = AgentOut {
    agentOutState :: AgentState
}

-- NOTE: need to provide an instance-implementation for NFData when using Par-Monad as it reduces to normal-form
instance NFData AgentState where
    rnf (AgentState p e f h) = rnf p `seq` rnf e `seq` rnf f `seq` rnf h

createAgents :: Int -> Double -> IO [AgentState]
createAgents n p = mapM (\id -> randomAgent id n p ) [0..n-1]

decidePosition :: AgentPosition -> AgentPosition -> AgentState -> AgentPosition
decidePosition friendPos enemyPos a = if hero a then cover friendPos enemyPos else hide friendPos enemyPos
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos

cover :: AgentPosition -> AgentPosition -> AgentPosition
cover friendPos enemyPos = newPos
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        newPos = addPos friendPos (multPos enemyFriendDir distance)

hide :: AgentPosition -> AgentPosition -> AgentPosition
hide friendPos enemyPos = newPos
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        newPos = subPos friendPos (multPos enemyFriendDir distance)

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------

distance :: Double
distance = 10.0

multPos :: AgentPosition -> Double -> AgentPosition
multPos (x, y) s = (x*s, y*s)

addPos :: AgentPosition -> AgentPosition -> AgentPosition
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subPos :: AgentPosition -> AgentPosition -> AgentPosition
subPos (x1, y1) (x2, y2) = (x1-x2, y1-y2)

posDir :: AgentPosition -> AgentPosition -> AgentPosition
posDir (v1x, v1y) (v2x, v2y) = (v2x - v1x, v2y - v1y)

vecLen :: AgentPosition -> Double
vecLen (vx, vy) = sqrt( vx * vx + vy * vy )

vecNorm :: AgentPosition -> AgentPosition
vecNorm (vx, vy) = (vx / len, vy / len)
    where
        len = vecLen (vx, vy)

randomAgent :: Int -> Int -> Double -> IO AgentState
randomAgent id maxAgents p =
    do
        randX <- getStdRandom( randomR(1.0, 800.0) )
        randY <- getStdRandom( randomR(1.0, 800.0) )
        randEnemy <- drawRandomIgnoring 0 (maxAgents-1) [id]
        randFriend <- drawRandomIgnoring 0 (maxAgents-1) [id, randEnemy]
        randHero <- randomThresh p
        return AgentState { agentPos = (randX, randY),
            enemy = randEnemy,
            friend = randFriend,
            hero = randHero}

drawRandomIgnoring :: Int -> Int -> [Int] -> IO Int
drawRandomIgnoring lowerRange upperRange ignore =
    do
        randInt <- getStdRandom(randomR(lowerRange, upperRange - 1))
        if any (==randInt) ignore then
            drawRandomIgnoring lowerRange upperRange ignore
                else
                    return randInt

randomThresh :: Double -> IO Bool
randomThresh p =
    do
        thresh <- getStdRandom( randomR(0.0, 1.0) )
        return (thresh <= p)