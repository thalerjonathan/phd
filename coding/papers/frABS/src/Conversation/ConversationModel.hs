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

type ConversationAgentConversation = AgentConversationReceiver ConversationAgentState ConversationMsg ConversationEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)
------------------------------------------------------------------------------------------------------------------------

conversationHandler :: ConversationAgentConversation
conversationHandler ain (senderId, msg@(Hello n)) =
    trace ("Agent " ++ (show $ aiId ain) ++ " receives conversation: " ++ (show msg))
        (Hello (n+1), ain)

makeConversationWith :: ConversationAgentOut -> ConversationAgentOut
makeConversationWith a = beginConversation a msg makeConversationWithAux
    where
        receiverId = if aoId a == 0 then 1 else 0
        msg = (receiverId, Hello 0)

        makeConversationWithAux :: ConversationAgentOut -> Maybe (AgentMessage ConversationMsg) -> ConversationAgentOut
        makeConversationWithAux a (Just (senderId, msg@(Hello n)))
            | senderId == 0 = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " but stoppin") stopConversation a
            | otherwise = if (n<2) then (beginConversation a (2, Hello (n + 1)) makeConversationWithAux) else stopConversation a

conversationAgentBehaviour :: ConversationAgentBehaviour
conversationAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< makeConversationWith ao