module Main where

import SIRS.RunSIRS

main :: IO ()
main = runSIRSWithRendering

{-
import FRP.Yampa
import System.IO
import Debug.Trace
import Agent.Agent

data TestAgentMsg = Increment | Decrement deriving (Show, Eq)
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

incrementFilter :: AgentMessage TestAgentMsg -> Bool
incrementFilter (_, Increment) = True
incrementFilter otherwise = False

decrementFilter :: AgentMessage TestAgentMsg -> Bool
decrementFilter (_, Decrement) = True
decrementFilter otherwise = False

testAgentBehaviour :: TestAgent -> TestAgentBehaviour
testAgentBehaviour aInit = proc ain ->
    do
        let ao = agentOutFromIn ain

        let msgs = [(0, Increment), (1, Increment), (2, Increment)]
        let ao' = sendMessages ao msgs

        let ao2 = onStart ain (\aoTemp -> trace ("Start-Event in " ++ (show (aiId ain))) aoTemp ) ao'

        let ao3 = onAnyMessage ain (\aoTemp (senderId, m) ->
                                        trace ("onAnyMessage in " ++
                                            (show (aiId ain)) ++ " from " ++ (show senderId) ++ ": " ++ (show m)) aoTemp ) ao2

        let ao4 = onMessageFrom 0 ain (\aoTemp (senderId, m) ->
                                        trace ("onMessageFrom in " ++
                                            (show (aiId ain)) ++ " from " ++ (show senderId) ++ ": " ++ (show m)) aoTemp ) ao3

        let ao4 = onMessageType Increment ain (\aoTemp (senderId, m) ->
                                        trace ("onMessageType in " ++
                                            (show (aiId ain)) ++ " from " ++ (show senderId) ++ ": " ++ (show m)) aoTemp ) ao3


        let ao5 = onMessage ain incrementFilter (\aoTemp (senderId, m) ->
                                        trace ("onMessage in " ++
                                            (show (aiId ain)) ++ " from " ++ (show senderId) ++ ": " ++ (show m)) aoTemp ) ao4

        let ao6 = updateState ao5 (\s -> s + 1)

        returnA -< ao6
-}