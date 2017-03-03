{-# LANGUAGE Arrows #-}

module Main where

import FRP.Yampa
import Debug.Trace
import Agent.Agent

data TestAgentMsg = Increment | Decrement deriving (Show)
type TestAgentState = Int
type TestAgent = AgentDef TestAgentState TestAgentMsg
type TestAgentBehaviour = AgentBehaviour TestAgentState TestAgentMsg

main :: IO ()
main = do
        let as = createAgents
        let as' = processSteps as 1.0 10
        let lastAs' = last as'
        putStrLn $ show (length lastAs')
        mapM printAgentOut lastAs'
        return ()


printAgentOut :: (Show s) => AgentOut s m -> IO ()
printAgentOut ao = do
                    let s = aoState ao
                    putStrLn $ "AgentOut: " ++ (show s)

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
testAgentBehaviour aInit = proc agentIn ->
    do
        let s = aiState agentIn
        let s' = s + 1
        let test = if (isEvent (aiStart agentIn) ) then
                        trace ("Start-Event in " ++ (show (aiId agentIn))) " Start"
                        else
                            " NoStart"

        returnA -< trace ("testAgentBehaviour in " ++ (show (aiId agentIn)) ++ (show test))
                            (AgentOut{ aoKill = NoEvent,
                                         aoCreate = [],
                                         aoMessages = [],
                                         aoState = s' })