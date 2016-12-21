module HeroesAndCowards.HACModel where

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.Random
import Data.Maybe

import qualified PureAgents as PA

-- TODO: fix parameters which won't change anymore after an Agent has started by using currying. e.g. World-Type

type HACAgentPosition = (Double, Double)
data HACMsg = PositionRequest | PositionUpdate HACAgentPosition
data HACWorldType = Infinite | Border | Wraping deriving (Eq, Show)

type HACEnvironment = Int

data HACAgentState = HACAgentState {
    pos :: HACAgentPosition,
    hero :: Bool,
    wt :: HACWorldType,
    friend :: PA.AgentId,
    enemy :: PA.AgentId,
    friendPos :: Maybe HACAgentPosition,
    enemyPos :: Maybe HACAgentPosition
} deriving (Show)

type HACAgent = PA.Agent HACMsg HACAgentState HACEnvironment
type HACSimHandle = PA.SimHandle HACMsg HACAgentState HACEnvironment

hacMovementPerTimeUnit :: Double
hacMovementPerTimeUnit = 1.0

hacMsgHandler :: HACAgent -> HACMsg -> PA.AgentId -> STM HACAgent
-- MESSAGE-CASE: PositionUpdate
hacMsgHandler a (PositionUpdate (x, y)) senderId
    | senderId == friendId = return a { PA.state = s { friendPos = newPos } }
    | senderId == enemyId = return a { PA.state = s { enemyPos = newPos } }
        where
            s = PA.state a
            friendId = friend s
            enemyId = enemy s
            newPos = Just (x,y)
-- MESSAGE-CASE: PositionRequest
hacMsgHandler a PositionRequest senderId = do
                                                PA.sendMsg a (PositionUpdate currPos) senderId
                                                return a
    where
        s = PA.state a
        currPos = pos s

hacUpdtHandler :: HACAgent -> Double -> STM HACAgent
hacUpdtHandler a dt = do
                        PA.changeEnv a (\e -> e + 1)
                        requestPosition a (friend s)
                        requestPosition a (enemy s)
                        if ((isJust fPos) && (isJust ePos)) then
                            return a { PA.state = s' }
                                else
                                    return a
    where
        s = PA.state a
        fPos = friendPos s
        ePos = enemyPos s
        oldPos = pos s
        targetPos = decidePosition (fromJust fPos) (fromJust ePos) (hero s)
        targetDir = vecNorm $ posDir oldPos targetPos
        wtFunc = worldTransform (wt s)
        stepWidth = hacMovementPerTimeUnit * dt
        newPos = wtFunc $ addPos oldPos (multPos targetDir stepWidth)
        s' = s{ pos = newPos }

requestPosition :: HACAgent -> PA.AgentId -> STM ()
requestPosition a receiverId = PA.sendMsg a PositionRequest receiverId

createHACTestAgents :: STM [HACAgent]
createHACTestAgents = do
                    let a0State = HACAgentState{ pos = (0.0, -0.5), hero = False, friend = 1, enemy = 2, wt = Border, friendPos = Nothing, enemyPos = Nothing }
                    let a1State = HACAgentState{ pos = (0.5, 0.5), hero = False, friend = 0, enemy = 2, wt = Border, friendPos = Nothing, enemyPos = Nothing }
                    let a2State = HACAgentState{ pos = (-0.5, 0.5), hero = False, friend = 0, enemy = 1, wt = Border, friendPos = Nothing, enemyPos = Nothing }
                    a0 <- PA.createAgent 0 a0State hacMsgHandler hacUpdtHandler
                    a1 <- PA.createAgent 1 a1State hacMsgHandler hacUpdtHandler
                    a2 <- PA.createAgent 2 a2State hacMsgHandler hacUpdtHandler
                    let a0' = PA.addNeighbours a0 [a1, a2]
                    let a1' = PA.addNeighbours a1 [a0, a2]
                    let a2' = PA.addNeighbours a2 [a0, a1]
                    return [a0', a1', a2']


createRandomHACAgents :: RandomGen g => g -> Int -> Double -> STM ([HACAgent], g)
createRandomHACAgents gInit n p = do
                                    as <- mapM (\idx -> PA.createAgent idx (randStates !! idx) hacMsgHandler hacUpdtHandler) [0..n-1]
                                    -- NOTE: this means that the agents could send messages to all other agents and not only to their friend and enemy
                                    let as' = map (\a -> PA.addNeighbours a as) as
                                    return (as', g')

                                      where
                                        (randStates, g') = createRandomStates gInit 0 n p

                                        createRandomStates :: RandomGen g => g -> Int -> Int -> Double -> ([HACAgentState], g)
                                        createRandomStates g id n p
                                          | id == n = ([], g)
                                          | otherwise = (rands, g'')
                                              where
                                                  (randState, g') = randomAgentState g id n p
                                                  (ras, g'') = createRandomStates g' (id+1) n p
                                                  rands = randState : ras

----------------------------------------------------------------------------------------------------------------------
-- PRIVATES
----------------------------------------------------------------------------------------------------------------------
randomAgentState :: (RandomGen g) => g -> Int -> Int -> Double -> (HACAgentState, g)
randomAgentState g id maxAgents p = (s, g5)
    where
        allAgentIds = [0..maxAgents-1]
        (randX, g') = randomR(-1.0, 1.0) g
        (randY, g'') = randomR(-1.0, 1.0) g'
        (randEnemy, g3) = drawRandomIgnoring g'' allAgentIds [id]
        (randFriend, g4) = drawRandomIgnoring g3 allAgentIds [id, randEnemy]
        (randHero, g5) = randomThresh g4 p
        s = HACAgentState{ pos = (randX, randY),
                            hero = randHero,
                            friend = randEnemy,
                            enemy = randFriend,
                            wt = Border,
                            friendPos = Nothing,
                            enemyPos = Nothing }

randomThresh :: (RandomGen g) => g -> Double -> (Bool, g)
randomThresh g p = (flag, g')
    where
        (thresh, g') = randomR(0.0, 1.0) g
        flag = thresh <= p

-- NOTE: this solution will recur forever if there are no possible solutions but will be MUCH faster for large xs and if xs is much larger than is and one knows there are solutions
drawRandomIgnoring :: (RandomGen g, Eq a) => g -> [a] -> [a] -> (a, g)
drawRandomIgnoring g xs is
    | any (==randElem) is = drawRandomIgnoring g' xs is
    | otherwise = (randElem, g')
        where
            (randIdx, g') = randomR(0, length xs - 1) g
            randElem = xs !! randIdx

decidePosition :: HACAgentPosition -> HACAgentPosition -> Bool -> HACAgentPosition
decidePosition friendPos enemyPos hero
    | hero = coverPosition
    | otherwise = hidePosition
    where
        enemyFriendDir = posDir friendPos enemyPos
        halfPos = multPos enemyFriendDir 0.5
        coverPosition = addPos friendPos halfPos
        hidePosition = subPos friendPos halfPos

multPos :: HACAgentPosition -> Double -> HACAgentPosition
multPos (x, y) s = (x*s, y*s)

addPos :: HACAgentPosition -> HACAgentPosition -> HACAgentPosition
addPos (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subPos :: HACAgentPosition -> HACAgentPosition -> HACAgentPosition
subPos (x1, y1) (x2, y2) = (x1-x2, y1-y2)

posDir :: HACAgentPosition -> HACAgentPosition -> HACAgentPosition
posDir (x1, y1) (x2, y2) = (x2-x1, y2-y1)

vecLen :: HACAgentPosition -> Double
vecLen (x, y) = sqrt( x * x + y * y )

vecNorm :: HACAgentPosition -> HACAgentPosition
vecNorm (x, y)
    | len == 0 = (0, 0)
    | otherwise = (x / len, y / len)
    where
        len = vecLen (x, y)

clip :: HACAgentPosition -> HACAgentPosition
clip (x, y) = (clippedX, clippedY)
    where
        clippedX = max (-1.0) (min x 1.0)
        clippedY = max (-1.0) (min y 1.0)

wrap :: HACAgentPosition -> HACAgentPosition
wrap (x, y) = (wrappedX, wrappedY)
    where
        wrappedX = wrapValue x
        wrappedY = wrapValue y

wrapValue :: Double -> Double
wrapValue v
    | v > 1.0 = -1.0
    | v < -1.0 = 1.0
    | otherwise = v

worldTransform :: HACWorldType -> (HACAgentPosition -> HACAgentPosition)
worldTransform wt
    | wt == Border = clip
    | wt == Wraping = wrap
    | otherwise = id