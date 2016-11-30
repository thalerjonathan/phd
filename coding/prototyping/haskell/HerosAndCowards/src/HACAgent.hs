module HACAgent (
    AgentId,
    AgentPosition,
    AgentState (..),
    AgentIn (..),
    AgentOut (..),
    agentSpeedPerTimeUnit,
    agentStep,
    createRandAgentStates,
    agentInFromAgents
  ) where

import System.Random
import Control.DeepSeq

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type AgentPosition = (Double, Double)

agentSpeedPerTimeUnit :: Double
agentSpeedPerTimeUnit = 0.005

data AgentState = AgentState {
    agentId :: AgentId,
    agentPos :: AgentPosition,
    enemy :: AgentId,
    friend :: AgentId,
    hero :: Bool
}

instance Show AgentState where
    show (AgentState agentId agentPos enemy friend hero) =
        "Agent " ++ (show agentId) ++
        " has friend " ++ (show friend) ++
        ", has enemy " ++ (show enemy) ++
        ", is on " ++ (show agentPos)

showAgents :: [AgentState] -> IO [()]
showAgents as =  mapM (putStrLn . show) as

data AgentIn = AgentIn {
    agentInState :: AgentState,
    agentInEnemyPos :: AgentPosition,
    agentInFriendPos :: AgentPosition
}

data AgentOut = AgentOut {
    agentOutState :: AgentState,
    agentOutDir :: AgentPosition
}

-- NOTE: need to provide an instance-implementation for NFData when using Par-Monad as it reduces to normal-form
instance NFData AgentState where
    rnf (AgentState id p e f h) = rnf id `seq` rnf p `seq` rnf e `seq` rnf f `seq` rnf h

-- NOTE: need to provide an instance-implementation for NFData when using Par-Monad as it reduces to normal-form
instance NFData AgentOut where
    rnf (AgentOut os od) = rnf os `seq` rnf od

createRandAgentStates :: RandomGen g => g -> Int -> Double -> [AgentState]
createRandAgentStates g n p = createAgents' g 0 n p
    where
        createAgents' g id n p
            | id == n = []
            | otherwise = randState : createAgents' g'' (id+1) n p
                where
                    randState = randomAgentState g' id n p
                    (g', g'') = split g

agentStep :: Double -> AgentIn -> AgentOut
agentStep stepWidth aIn = AgentOut { agentOutState = a { agentPos = newPos }, agentOutDir = targetDir }
    where
        a = agentInState aIn
        friendPos = agentInFriendPos aIn
        enemyPos = agentInEnemyPos aIn
        oldPos = agentPos a
        targetPos = decidePosition friendPos enemyPos a
        targetDir = vecNorm $ posDir oldPos targetPos
        newPos = addPos oldPos (multPos targetDir stepWidth)

agentInFromAgents :: [AgentState] -> [AgentIn]
agentInFromAgents as = map agentInFromAgents' as
    where
        agentInFromAgents' :: AgentState -> AgentIn
        agentInFromAgents' a = AgentIn { agentInState = a, agentInEnemyPos = enemyPos, agentInFriendPos = friendPos }
            where
                friendPos = agentPos (as !! friend a)
                enemyPos = agentPos (as !! enemy a)

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
hideDistance :: Double
hideDistance = 0.1

coverDistance :: Double
coverDistance = 0.1

decidePosition :: AgentPosition -> AgentPosition -> AgentState -> AgentPosition
decidePosition friendPos enemyPos a
    | hero a = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        coverPosition = addPos friendPos (multPos enemyFriendDir coverDistance)
        hidePosition = subPos friendPos (multPos enemyFriendDir hideDistance)

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
        (randX, g') = randomR(0.0, 1.0) g
        (randY, g'') = randomR(0.0, 1.0) g'
        (randEnemy, g''') = drawRandomIgnoring g'' 0 (maxAgents-1) [id]
        (randFriend, g4) = drawRandomIgnoring g''' 0 (maxAgents-1) [id, randEnemy]
        (randHero, g5) = randomThresh g4 p
        a = AgentState { agentId = id,
                        agentPos = (randX, randY),
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