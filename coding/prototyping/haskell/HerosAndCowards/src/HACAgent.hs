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

data Agent a = Agent a

instance Functor Agent where
    fmap f (Agent a) = Agent (f a)

instance Applicative Agent where
    pure a = Agent a
    (Agent f) <*> a = fmap f a

instance Monad Agent where
    return = pure
    Agent a >>= f = f a

createRandAgents :: RandomGen g => g -> Int -> Double -> [Agent AgentState]
createRandAgents g n p = createRandAgents' g 0 n p
    where
        createRandAgents' g id n p
            | id == n = []
            | otherwise = randAgent : createRandAgents' g'' (id+1) n p
                where
                    randState = randomAgentState g' id n p
                    randAgent = (return randState) :: Agent AgentState
                    (g', g'') = split g


createAgents :: RandomGen g => g -> Int -> Double -> [AgentState]
createAgents g n p = createAgents' g 0 n p
    where
        createAgents' g id n p
            | id == n = []
            | otherwise = randState : createAgents' g'' (id+1) n p
                where
                    randState = randomAgentState g' id n p
                    (g', g'') = split g

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

randomAgentState :: RandomGen g => g -> Int -> Int -> Double -> AgentState
randomAgentState g id maxAgents p = a
    where
        (randX, g') = randomR(1.0, 800.0) g
        (randY, g'') = randomR(1.0, 800.0) g'
        (randEnemy, g''') = drawRandomIgnoring g'' 0 (maxAgents-1) [id]
        (randFriend, g4) = drawRandomIgnoring g''' 0 (maxAgents-1) [id, randEnemy]
        (randHero, g5) = randomThresh g4 p
        a = AgentState { agentPos = (randX, randY),
                        enemy = randEnemy,
                        friend = randFriend,
                        hero = randHero}

drawRandomIgnoring :: (RandomGen g) => g -> Int -> Int -> [Int] -> (Int, g)
drawRandomIgnoring g lowerRange upperRange ignore
    | any (==randInt) ignore = drawRandomIgnoring g' lowerRange upperRange ignore
    | otherwise = (randInt, g')
        where
            (randInt, g') = randomR(lowerRange, upperRange - 1) g

randomThresh :: (RandomGen g) => g -> Double -> (Bool, g)
randomThresh g p = (flag, g')
    where
        (thresh, g') = randomR(0.0, 1.0) g
        flag = thresh <= p