{-# LANGUAGE Arrows #-}
module Conversation.ConversationModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment
import FrABS.Simulation.Simulation

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data ConversationMsg = Hello Int deriving (Eq, Show)

data ConversationAgentState = ConversationAgentState {
    convCounter :: Int,
    convRng :: StdGen
} deriving (Show)

type ConversationEnvCell = ()
type ConversationEnvironment = Environment ConversationEnvCell

type ConversationAgentDef = AgentDef ConversationAgentState ConversationMsg ConversationEnvCell
type ConversationAgentBehaviour = AgentBehaviour ConversationAgentState ConversationMsg ConversationEnvCell
type ConversationAgentIn = AgentIn ConversationAgentState ConversationMsg ConversationEnvCell
type ConversationAgentOut = AgentOut ConversationAgentState ConversationMsg ConversationEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)
------------------------------------------------------------------------------------------------------------------------

conversationAgentBehaviour :: ConversationAgentBehaviour
conversationAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< ao