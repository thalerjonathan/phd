module HACAgent (
    AgentId,
    AgentPosition,
    AgentState (..),
    AgentIn (..),
    AgentOut (..),
    WorldType (..),
    showAgents,
    agentStep,
    createRandAgentStates,
    agentInFromAgents
  ) where

import System.Random
import Control.DeepSeq

import Utils
----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type AgentPosition = (Double, Double)

data WorldType = Infinite | Border | Wraping | InfiniteWraping deriving (Eq)

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

data AgentIn = AgentIn {
    agentInState :: AgentState,
    agentInEnemyPos :: AgentPosition,
    agentInFriendPos :: AgentPosition
}

data AgentOut = AgentOut {
    agentOutState :: AgentState,
    agentOutDir :: AgentPosition
}

instance Eq AgentState where
    a1 == a2 = (agentId a1) == (agentId a2)

-- NOTE: need to provide an instance-implementation for NFData when using Par-Monad as it reduces to normal-form
instance NFData AgentState where
    rnf (AgentState id p e f h) = rnf id `seq` rnf p `seq` rnf e `seq` rnf f `seq` rnf h

-- NOTE: need to provide an instance-implementation for NFData when using Par-Monad as it reduces to normal-form
instance NFData AgentOut where
    rnf (AgentOut os od) = rnf os `seq` rnf od

agentSpeedPerTimeUnit :: Double
agentSpeedPerTimeUnit = 0.1

showAgents :: [AgentState] -> IO [()]
showAgents as =  mapM (putStrLn . show) as

createRandAgentStates :: RandomGen g => g -> Int -> Double -> [AgentState]
createRandAgentStates g n p = createAgents' g 0 n p
    where
        createAgents' g id n p
            | id == n = []
            | otherwise = randState : createAgents' g'' (id+1) n p
                where
                    randState = randomAgentState g' id n p
                    (g', g'') = split g

agentStep :: WorldType -> Double -> AgentIn -> AgentOut
agentStep wt dt aIn = AgentOut { agentOutState = a { agentPos = newPos }, agentOutDir = targetDir }
    where
        a = agentInState aIn
        friendPos = agentInFriendPos aIn
        enemyPos = agentInEnemyPos aIn
        oldPos = agentPos a
        targetPos = decidePosition friendPos enemyPos a
        targetDir = vecNorm $ posDir oldPos targetPos
        wtFunc = worldTransform wt
        stepWidth = agentSpeedPerTimeUnit * dt
        newPos = wtFunc $ addPos oldPos (multPos targetDir stepWidth)

agentInFromAgents :: [AgentState] -> [AgentIn]
agentInFromAgents as = map agentInFromAgents' as
    where
        agentInFromAgents' :: AgentState -> AgentIn
        agentInFromAgents' a = AgentIn { agentInState = a, agentInEnemyPos = enemyPos, agentInFriendPos = friendPos }
            where
                -- NOTE: the more agents (as) there are, the more time the next two lines consume. In case of 3000 Agents
                --       on my working machine in the office, each of them consumes between 39-44% of the runtime thus
                --       making up of roughly 80% of the time
                friendPos = agentPos (as !! friend a)
                enemyPos = agentPos (as !! enemy a)

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
decidePosition :: AgentPosition -> AgentPosition -> AgentState -> AgentPosition
decidePosition friendPos enemyPos a
    | hero a = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendDir = posDir friendPos enemyPos
        coverPosition = addPos friendPos (multPos enemyFriendDir 0.5)
        hidePosition = subPos friendPos (multPos enemyFriendDir 0.5)


multPos :: AgentPosition -> Double -> AgentPosition
multPos (x, y) s = (x*s, y*s)

addPos :: AgentPosition -> AgentPosition -> AgentPosition
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subPos :: AgentPosition -> AgentPosition -> AgentPosition
subPos (x1, y1) (x2, y2) = (x1-x2, y1-y2)

posDir :: AgentPosition -> AgentPosition -> AgentPosition
posDir (v1x, v1y) (v2x, v2y) = (v2x-v1x, v2y-v1y)

vecLen :: AgentPosition -> Double
vecLen (vx, vy) = sqrt( vx * vx + vy * vy )

vecNorm :: AgentPosition -> AgentPosition
vecNorm (vx, vy)
    | len == 0 = (0, 0)
    | otherwise = (vx / len, vy / len)
    where
        len = vecLen (vx, vy)

clip :: AgentPosition -> AgentPosition
clip (x, y) = (clippedX, clippedY)
    where
        clippedX = max 0.0 (min x 1.0)
        clippedY = max 0.0 (min y 1.0)

wrap :: AgentPosition -> AgentPosition
wrap (x, y) = (wrappedX, wrappedY)
    where
        wrappedX = wrapValue x
        wrappedY = wrapValue y

wrapValue :: Double -> Double
wrapValue v
    | v > 1.0 = 0.0
    | v < 0.0 = 1.0
    | otherwise = v

worldTransform :: WorldType -> (AgentPosition -> AgentPosition)
worldTransform wt
    | wt == Border = clip
    | wt == Wraping = wrap
    | otherwise = id


randomAgentState :: RandomGen g => g -> Int -> Int -> Double -> AgentState
randomAgentState g id maxAgents p = a
    where
        allAgentIds = [0..maxAgents-1]
        (randX, g') = randomR(0.0, 1.0) g
        (randY, g'') = randomR(0.0, 1.0) g'
        (randEnemy, g3) = Utils.drawRandomIgnoring g'' allAgentIds [id]
        (randFriend, g4) = Utils.drawRandomIgnoring g3 allAgentIds [id, randEnemy]
        (randHero, _) = randomThresh g4 p
        a = AgentState { agentId = id,
                        agentPos = (randX, randY),
                        enemy = randEnemy,
                        friend = randFriend,
                        hero = randHero}

randomThresh :: (RandomGen g) => g -> Double -> (Bool, g)
randomThresh g p = (flag, g')
    where
        (thresh, g') = randomR(0.0, 1.0) g
        flag = thresh <= p
