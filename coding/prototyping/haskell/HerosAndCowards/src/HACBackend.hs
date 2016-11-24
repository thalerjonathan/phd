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
process initAgentStates = proc simIn ->
    do
        rec
            agentOuts <- (procHelper initAgentStates) -< (simIn, agentStates)
            let agentStates = map agentOutState agentOuts
        let agentPositions = map agentPos agentStates
        returnA -< SimulationOut{ simOutAllAgents = agentPositions }

procHelper :: [AgentState] -> SF (SimulationIn, [AgentState]) [AgentOut]
procHelper agentStates = dpSwitch
                            route
                            (agentsToSF agentStates)
                            (arr collectOutput >>> notYet)              -- Signal function that observes the external input signal and the output signals from the collection in order to produce a switching event.
                            continuation

{- Routing function. Its purpose is to pair up each running signal function
in the collection maintained by par with the input it is going to see
at each point in time. All the routing function can do is specify how the input is distributed.

1st argument:   the input
2nd argument:   the collection of signal-functions

returns:        a list of tuples where:
                    1st item of the tuple is the input to the signal-function a
                    2nd item is the signal-function
-}
route :: (SimulationIn, [AgentState]) -> [sf] -> [(AgentIn, sf)]
route (simIn, agentStates) agentSFs = map (\sf -> (AgentIn { agentInAgents = agentPositions }, sf)) agentSFs
    where
        agentPositions = map agentPos agentStates

-- creates the initial collection of signal functions.
agentsToSF :: [AgentState] -> [ActiveAgent]
agentsToSF agents = map activeAgent agents

{- Signal function that observes the external input signal and
the output signals from the collection in order to produce a switching event.

1st argument:   tuple where the first is the input to the process SF and the second is the output of the running SFs
return:         an event with arbitrary data
-}
collectOutput :: ((SimulationIn, [AgentState]), [AgentOut]) -> (Event [AgentState])
collectOutput ((simIn, oldAgentStates), newAgentOuts) = Event (map agentOutState newAgentOuts)

{- The fourth argument is a function that is invoked when the switching event occurs,
yielding a new signal function to switch into based on the collection of signal functions
previously running and the value carried by the switching event. This allows the collection
 to be updated and then switched back in, typically by employing dpSwitch again.
-}
continuation :: [ActiveAgent] -> [AgentState] -> SF (SimulationIn, [AgentState]) [AgentOut]
continuation agentSFs newAgentStates = procHelper newAgentStates

activeAgent :: AgentState -> ActiveAgent
activeAgent a = proc agentIn ->
    do
        let friendPos = agentInAgents agentIn !! friend a
        let enemyPos = agentInAgents agentIn !! enemy a
        let enemyFriendDir = vecNorm $ posDir friendPos enemyPos
        let newPos = if ishero a then coverPosition friendPos enemyPos else hidePosition friendPos enemyPos
        newXCoord <- integral -< (fst newPos)
        newYCoord <- integral -< (snd newPos)
        returnA -< AgentOut{ agentOutState = a { agentPos = (newXCoord, newYCoord) } }

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
        randEnemy <- drawRandomIgnoring 0 (maxAgents-1) [id]
        randFriend <- drawRandomIgnoring 0 (maxAgents-1) [id, randEnemy]
        randHero <- randomThresh 0.0
        return AgentState { agentPos = (randX, randY),
            enemy = randEnemy,
            friend = randFriend,
            ishero = randHero}

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
        return (thresh >= p)