module HACSimulation where

import HACAgent as Agent

data SimIn = SimIn {
}

data SimOut = SimOut {
    simOutAllAgents :: [Agent.AgentOut]
}
