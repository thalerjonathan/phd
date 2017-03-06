{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import System.IO
import Debug.Trace
import Agent.Agent

data TestAgentMsg = Increment | Decrement deriving (Show)
type TestAgentState = Int
type TestAgent = AgentDef TestAgentState TestAgentMsg
type TestAgentBehaviour = AgentBehaviour TestAgentState TestAgentMsg

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        hSetBuffering stderr NoBuffering
        let as = createAgents
        -- processIO as ioFun
        runSteps as 2

runSteps :: [TestAgent] -> Int -> IO ()
runSteps as steps = do
                        let aos = processSteps as 1.0 steps
                        let finalAos = last aos
                        printAgentOuts finalAos

ioFun :: (Show s) => [AgentOut s m] -> IO (Bool, Double)
ioFun aos = do
                printAgentOuts aos
                return (True, 1.0)

printAgentOuts :: (Show s) => [AgentOut s m] -> IO ()
printAgentOuts aos = do
                        mapM printAgentOuts' aos
                        return ()
    where
        printAgentOuts' :: (Show s) => AgentOut s m -> IO ()
        printAgentOuts' ao = putStrLn $ "AgentOut: " ++ (show (aoState ao))

createAgents :: [TestAgent]
createAgents = [a0, a1, a2]
    where
        a0 = AgentDef { adId = 0,
                        adState = 0,
                        adBehaviour = testAgentBehaviour a0 }

        a1 = AgentDef { adId = 1,
                        adState = 1,
                        adBehaviour = testAgentBehaviour a1 }

        a2 = AgentDef { adId = 2,
                        adState = 2,
                        adBehaviour = testAgentBehaviour a2 }

testAgentBehaviour :: TestAgent -> TestAgentBehaviour
testAgentBehaviour aInit = proc ain ->
    do
        let ao = agentOutFromIn ain

        let msgs = [(0, Increment), (1, Increment), (2, Increment)]
        let ao' = sendMessages ao msgs

        let ao2 = onStart ain (\aoTemp -> trace ("Start-Event in " ++ (show (aiId ain))) aoTemp ) ao'

        let ao3 = onAnyMessage ain (\aoTemp (senderId, m) ->
                                        trace ("Received Message in " ++
                                            (show (aiId ain)) ++ " from " ++ (show senderId) ++ ": " ++ (show m)) aoTemp ) ao2

        returnA -< ao3

{-
testAgentBehaviour :: TestAgent -> TestAgentBehaviour
testAgentBehaviour aInit = proc ain ->
    do
        let s = aiState ain
        let s' = s + 1
        let test = if (isEvent (aiStart ain) ) then
                        trace ("Start-Event in " ++ (show (aiId agentIn))) " Start"
                        else
                            " NoStart"

        let msgs = [(0, Increment), (1, Increment), (2, Increment)]
        let ao = agentOutFromIn ain
        let ao' = sendMessages ao msgs

        returnA -< trace ("testAgentBehaviour in " ++ (show (aiId agentIn)) ++ (show test)) ao'
        -}