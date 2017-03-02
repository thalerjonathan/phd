module Main where

import Agent.Agent

main :: IO ()
main = do
        let as =

type TestAgentMsg = Increment | Decrement
type TestAgentState = Int
type TestAgent = Agent TestAgentState TestAgentMsg
type TestAgentBehaviour = AgentBehaviour TestAgentState TestAgentMsg

createAgents :: [TestAgent]
createAgents = [a0, a1, a2]
    where
        a0 = TestAgent { adId = 0,
                        adState = 0,
                        adBehaviour = testAgentBehaviour a0 }

        a1 = TestAgent { adId = 1,
                        adState = 0,
                        adBehaviour = testAgentBehaviour a1 }

        a2 = TestAgent { adId = 2,
                        adState = 0,
                        adBehaviour = testAgentBehaviour a2 }

testAgentBehaviour :: TestAgent -> TestAgentBehaviour
testAgentBehaviour aInit = proc agentIn ->
    do
        returnA -< AgentOut{ aoState = (aiState agentIn) }