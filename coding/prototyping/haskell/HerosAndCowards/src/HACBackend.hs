{-# LANGUAGE Arrows #-}
module HACBackend where

import System.Random

import FRP.Yampa
import FRP.Yampa.Switches

type AgentId = Int
type AgentPosition = (Double, Double)

data AgentState = AgentState {
    agentPos :: AgentPosition,
    enemy :: AgentId,
    friend :: AgentId,
    ishero :: Bool
}

data SimulationIn = SimulationIn {

}

data SimulationOut = SimulationOut {
    simOutAllAgents :: [AgentPosition]
}

data AgentIn = AgentIn {
    agentInAgents :: [AgentPosition]
}

data AgentOut = AgentOut {
    agentOutState :: AgentState
}

type ActiveAgent = SF AgentIn AgentOut

process :: [AgentState] -> SF SimulationIn SimulationOut
process initAgents = proc simIn ->
    do
        agents <- (procHelper initAgents) -< simIn
        let agentPositions = map (agentPos . agentOutState) agents
        returnA -< SimulationOut{ simOutAllAgents = agentPositions }

procHelper :: [AgentState] -> SF SimulationIn [AgentOut]
procHelper agents = par route (agentsToSF agents)

{- Routing function. Its purpose is to pair up each running signal function
in the collection maintained by dpSwitch with the input it is going to see
at each point in time. All the routing function can do is specify how the input is distributed.
-}
route :: SimulationIn -> [sf] -> [(AgentIn, sf)]
route simIn activeAgents = map (\sf -> (AgentIn { agentInAgents = [] }, sf)) activeAgents

-- creates the initial collection of signal functions.
agentsToSF :: [AgentState] -> [ActiveAgent]
agentsToSF agents = map activeAgent agents

activeAgent :: AgentState -> ActiveAgent
activeAgent as = proc agentIn ->
    do
        let friendPos = agentInAgents agentIn !! friend as
        let enemyPos = agentInAgents agentIn !! enemy as
        let enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        let newPos = if ishero as then coverPosition friendPos enemyPos else hidePosition friendPos enemyPos
        newXCoord <- integral >>^ (+ (fst $ agentPos as)) -< -1.0
        newYCoord <- integral >>^ (+ (snd $ agentPos as)) -< -1.0
        returnA -< AgentOut{ agentOutState = as { agentPos = (newXCoord, newYCoord) } }

coverPosition :: AgentPosition -> AgentPosition -> AgentPosition
coverPosition friendPos enemyPos = newPos
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        newPos = addPos friendPos (multPos enemyFriendDir 10)

hidePosition :: AgentPosition -> AgentPosition -> AgentPosition
hidePosition friendPos enemyPos = newPos
    where
        enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        newPos = subPos friendPos (multPos enemyFriendDir 10)

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

createAgents :: Int -> IO [AgentState]
createAgents n = mapM (\id -> randomAgent id n ) [0..n-1]

randomAgent :: Int -> Int -> IO AgentState
randomAgent id maxAgents =
    do
        randX <- getStdRandom( randomR(1.0, 800.0) )
        randY <- getStdRandom( randomR(1.0, 800.0) )
        randEnemy <- getStdRandom( randomR(0, maxAgents) )
        randFriend <- getStdRandom( randomR(0, maxAgents) )
        randHero <- randomThresh 0.0
        return AgentState { agentPos = (randX, randY),
            enemy = randEnemy,
            friend = randFriend,
            ishero = randHero}

randomThresh :: Double -> IO Bool
randomThresh p =
    do
        thresh <- getStdRandom( randomR(0.0, 1.0) )
        return (thresh >= p)