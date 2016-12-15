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

data WorldType = Infinite | Border | Wraping deriving (Eq)

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
    agentInAllAgents :: [AgentState]
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
agentSpeedPerTimeUnit = 1.0

showAgents :: [AgentState] -> IO [()]
showAgents as =  mapM (putStrLn . show) as

createRandAgentStates :: RandomGen g => g -> Int -> Double -> ([AgentState], g)
createRandAgentStates gInit n p = createAgents' gInit 0 n p
    where
        createAgents' :: RandomGen g => g -> Int -> Int -> Double -> ([AgentState], g)
        createAgents' g id n p
            | id == n = ([], g)
            | otherwise = (rands, g'')
                where
                    (randState, g') = randomAgentState g id n p
                    (ras, g'') = createAgents' g' (id+1) n p
                    rands = randState : ras

agentStep :: WorldType -> Double -> AgentIn -> AgentOut
agentStep wt dt aIn = AgentOut { agentOutState = a { agentPos = newPos }, agentOutDir = targetDir }
    where
        a = agentInState aIn
        as = agentInAllAgents aIn
        friendPos = agentPos (as !! friend a)
        enemyPos = agentPos (as !! enemy a)
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
        agentInFromAgents' a = AgentIn { agentInState = a, agentInAllAgents = as }
----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
decidePosition :: AgentPosition -> AgentPosition -> AgentState -> AgentPosition
decidePosition friendPos enemyPos a
    | hero a = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendDir = posDir friendPos enemyPos
        halfPos = multPos enemyFriendDir 0.5
        coverPosition = addPos friendPos halfPos
        hidePosition = subPos friendPos halfPos

multPos :: AgentPosition -> Double -> AgentPosition
multPos (x, y) s = (x*s, y*s)

addPos :: AgentPosition -> AgentPosition -> AgentPosition
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subPos :: AgentPosition -> AgentPosition -> AgentPosition
subPos (x1, y1) (x2, y2) = (x1-x2, y1-y2)

posDir :: AgentPosition -> AgentPosition -> AgentPosition
posDir (x1, y1) (x2, y2) = (x2-x1, y2-y1)

vecLen :: AgentPosition -> Double
vecLen (x, y) = sqrt( x * x + y * y )

vecNorm :: AgentPosition -> AgentPosition
vecNorm (x, y)
    | len == 0 = (0, 0)
    | otherwise = (x / len, y / len)
    where
        len = vecLen (x, y)

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

randomAgentState :: RandomGen g => g -> Int -> Int -> Double -> (AgentState, g)
randomAgentState g id maxAgents p = (a, g5)
    where
        allAgentIds = [0..maxAgents-1]
        (randX, g') = randomR(0.0, 1.0) g
        (randY, g'') = randomR(0.0, 1.0) g'
        (randEnemy, g3) = Utils.drawRandomIgnoring g'' allAgentIds [id]
        (randFriend, g4) = Utils.drawRandomIgnoring g3 allAgentIds [id, randEnemy]
        (randHero, g5) = randomThresh g4 p
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
