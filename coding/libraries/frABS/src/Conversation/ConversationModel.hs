{-# LANGUAGE Arrows #-}
module Conversation.ConversationModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then

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
agentTest :: ConversationAgentOut -> ConversationAgentOut
agentTest a = updateState a (\s -> s { convRng = g', convCounter = n})
    where
        g = convRng $ aoState a
        (n, g') = randomR (0, 10) g

conversationHandler :: ConversationAgentConversation
conversationHandler ain (_, msg@(Hello n)) =
    trace ("Agent " ++ (show $ aiId ain) ++ " receives conversation: " ++ (show msg))
        Just (Hello (n+1), ain)

makeConversationWith :: Int -> ConversationAgentOut -> ConversationAgentOut
makeConversationWith n a = beginConversation a msg makeConversationWithAux
    where
        receiverId = if aoId a == 0 then 1 else 0
        msg =  trace ("makeConversationWith " ++ (show n) ++ " receiverId = " ++ (show receiverId))  (receiverId, Hello n)

        makeConversationWithAux :: ConversationAgentOut -> Maybe (AgentMessage ConversationMsg) -> ConversationAgentOut
        makeConversationWithAux a (Just (senderId, msg@(Hello n)))
            | n > 5 = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " but stoppin") stopConversation a
            | otherwise = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " continuing") makeConversationWith (n + 1) a
        makeConversationWithAux a _ = trace ("Agent " ++ (show $ aoId a) ++ " receives Nothing -> stopping") stopConversation a
{-
        makeConversationWithAux :: ConversationAgentOut -> Maybe (AgentMessage ConversationMsg) -> ConversationAgentOut
        makeConversationWithAux a (Just (senderId, msg@(Hello n)))
            | senderId == 0 = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " but stoppin") stopConversation a
            | otherwise = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " continuing") (if (n<2) then (beginConversation a (2, Hello (n + 1)) makeConversationWithAux) else stopConversation a)
        makeConversationWithAux a _ = trace ("Agent " ++ (show $ aoId a) ++ " receives Nothing -> stopping")  stopConversation a
-}

conversationAgentBehaviour :: ConversationAgentBehaviour
conversationAgentBehaviour = proc ain ->
    do
        let ao = trace ("conversationAgentBehaviour")  agentOutFromIn ain
        returnA -< makeConversationWith 0 ao