module HACSimulation where

import HACAgent as Agent

data SimIn = SimIn {
    simInAllAgents :: [Agent.AgentState]
}

data SimOut = SimOut {
    simOutAllAgents :: [Agent.AgentOut]
}
