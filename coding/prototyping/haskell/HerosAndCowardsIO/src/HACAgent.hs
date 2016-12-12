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

import System.IO.Unsafe
import Data.IORef
import System.Random
import Control.DeepSeq

----------------------------------------------------------------------------------------------------------------------
-- EXPORTS
----------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type AgentPosition = (Double, Double)

data WorldType = Infinite | Border | Wraping | InfiniteWraping deriving (Eq)

data AgentState = AgentState {
    agentId :: AgentId,
    agentPos :: IORef AgentPosition,
    enemy :: AgentId,
    friend :: AgentId,
    hero :: Bool
}

instance Show AgentState where
    show (AgentState agentId agentPos enemy friend hero) =
        "Agent " ++ (show agentId) ++
        " has friend " ++ (show friend) ++
        ", has enemy " ++ (show enemy) ++
        ", is on " ++ (show $ (unsafePerformIO $ readIORef agentPos))

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

createRandAgentStates :: Int -> Double -> IO [AgentState]
createRandAgentStates n p = mapM (randomAgentState n p) [0..n-1]

agentStep :: WorldType -> Double -> AgentIn -> IO AgentOut
agentStep wt dt aIn = do
    let posRef = agentPos a
    oldPos <- readIORef posRef
    friendPos <- readIORef friendPosRef
    enemyPos <- readIORef enemyPosRef
    let targetPos = decidePosition friendPos enemyPos a
    let targetDir = vecNorm $ posDir oldPos targetPos
    let wtFunc = worldTransform wt
    let stepWidth = agentSpeedPerTimeUnit * dt
    let newPos = wtFunc $ addPos oldPos (multPos targetDir stepWidth)
    writeIORef posRef newPos
    return AgentOut { agentOutState = a, agentOutDir = targetDir }
    where
        a = agentInState aIn
        as = agentInAllAgents aIn
        friendPosRef = agentPos (as !! friend a)
        enemyPosRef = agentPos (as !! enemy a)

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

randomAgentState :: Int -> Double -> Int -> IO AgentState
randomAgentState maxAgents p id = do
        stdGen <- getStdGen
        randX <- getStdRandom (randomR(0.0, 1.0))
        randY <- getStdRandom (randomR(0.0, 1.0))
        randEnemy <- drawRandomIgnoring allAgentIds [id]
        randFriend <- drawRandomIgnoring allAgentIds [id, randEnemy]
        randHero <- randomThresh p
        posRef <- newIORef (randX, randY)
        let a = AgentState { agentId = id,
                                    agentPos = posRef,
                                    enemy = randEnemy,
                                    friend = randFriend,
                                    hero = randHero}
        return a
    where
        allAgentIds = [0..maxAgents-1]

randomThresh :: Double -> IO Bool
randomThresh p = do
    thresh <- getStdRandom (randomR(0.0, 1.0))
    return (thresh <= p)

drawRandomIgnoring :: (Eq a) => [a] -> [a] -> IO a
drawRandomIgnoring xs is = do
    randIdx <- getStdRandom( randomR(0, length xs - 1))
    let randElem = xs !! randIdx
    if (any (==randElem) is) then
        drawRandomIgnoring xs is
        else
            return randElem
