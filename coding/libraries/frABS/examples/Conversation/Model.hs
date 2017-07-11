{-# LANGUAGE Arrows #-}
module Conversation.Model (
    ConversationMsg (..),
    ConversationAgentState,

    ConversationEnvCell,
    ConversationEnvironment,

    ConversationAgentDef,
    ConversationAgentBehaviour,
    ConversationAgentIn,
    ConversationAgentOut,

    ConversationAgentConversation,

    randomRangeCounter,
    conversationHandler,
    conversationAgentBehaviour
  ) where

import FrABS.Agent.Agent
import FrABS.Agent.Random
import FrABS.Env.Environment

import FRP.Yampa

import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data ConversationMsg = Hello Int deriving (Eq, Show)
type ConversationAgentState = Int

type ConversationEnvCell = ()
type ConversationEnvironment = Environment ConversationEnvCell  ()

type ConversationAgentDef = AgentDef ConversationAgentState ConversationMsg ConversationEnvCell ()
type ConversationAgentBehaviour = AgentBehaviour ConversationAgentState ConversationMsg ConversationEnvCell ()
type ConversationAgentIn = AgentIn ConversationAgentState ConversationMsg ConversationEnvCell ()
type ConversationAgentOut = AgentOut ConversationAgentState ConversationMsg ConversationEnvCell ()

type ConversationAgentConversation = AgentConversationReceiver ConversationAgentState ConversationMsg ConversationEnvCell ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL IMPLEMENTATION
------------------------------------------------------------------------------------------------------------------------
agentTest :: ConversationAgentOut -> ConversationAgentOut
agentTest a = setDomainState a' n
    where
        (n, a') = drawRandomRangeFromAgent a  (0, 10)

conversationHandler :: ConversationAgentConversation
conversationHandler ain (_, msg@(Hello n)) =
    trace ("Agent " ++ (show $ aiId ain) ++ " receives conversation: " ++ (show msg))
        Just (Hello (n+1), ain)

makeConversationWith :: Int -> ConversationAgentOut -> ConversationAgentOut
makeConversationWith n a = conversation a msg makeConversationWithAux
    where
        receiverId = if aoId a == 0 then 1 else 0
        msg =  trace ("makeConversationWith " ++ (show n) ++ " receiverId = " ++ (show receiverId))  (receiverId, Hello n)

        makeConversationWithAux :: ConversationAgentOut -> Maybe (AgentMessage ConversationMsg) -> ConversationAgentOut
        makeConversationWithAux a (Just (senderId, msg@(Hello n)))
            | n > 5 = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " but stoppin") conversationEnd a1
            | otherwise = trace ("Agent " ++ (show $ aoId a) ++ " receives reply: " ++ (show msg) ++ " continuing") makeConversationWith (n + 1) a
            where
                (g, a0) = splitRandomFromAgent a

                s = 42

                adef = AgentDef { adId = 100+n,
                            adState = s,
                            adEnvPos = (0,0),
                            adInitMessages = NoEvent,
                            adConversation = Just conversationHandler,
                            adBeh = conversationAgentBehaviour,
                            adRng = g }

                a1 = createAgent a0 adef 

        makeConversationWithAux a _ = trace ("Agent " ++ (show $ aoId a) ++ " receives Nothing -> stopping") conversationEnd a

conversationAgentBehaviour :: ConversationAgentBehaviour
conversationAgentBehaviour = proc ain ->
    do
        let ao = trace ("conversationAgentBehaviour")  agentOutFromIn ain
        returnA -< makeConversationWith 0 ao
------------------------------------------------------------------------------------------------------------------------