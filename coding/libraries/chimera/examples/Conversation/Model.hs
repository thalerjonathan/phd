{-# LANGUAGE Arrows #-}
module Conversation.Model (
    ConversationMsg (..),
    ConversationAgentState,

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

import FRP.FrABS

import FRP.Yampa

import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data ConversationMsg = Hello Int deriving (Eq, Show)
type ConversationAgentState = Int

type ConversationEnvironment = ()

type ConversationAgentDef = AgentDef ConversationAgentState ConversationMsg ConversationEnvironment
type ConversationAgentBehaviour = AgentBehaviour ConversationAgentState ConversationMsg ConversationEnvironment
type ConversationAgentIn = AgentIn ConversationAgentState ConversationMsg ConversationEnvironment
type ConversationAgentOut = AgentOut ConversationAgentState ConversationMsg ConversationEnvironment

type ConversationAgentConversation = AgentConversationReceiver ConversationAgentState ConversationMsg ConversationEnvironment
type ConversationAgentSender = AgentConversationSender ConversationAgentState ConversationMsg ConversationEnvironment
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
agentTest ao = setAgentState n ao'
    where
        (n, ao') = agentRandomRange (0, 10) ao

conversationHandler :: ConversationAgentConversation
conversationHandler ain e (_, msg@(Hello n)) =
    trace ("Agent " ++ (show $ agentId ain) ++ " receives conversation: " ++ (show msg))
        Just (agentStateIn ain, Hello (n+1), e)

makeConversationWith :: Int -> ConversationAgentOut -> ConversationAgentOut
makeConversationWith n ao = conversation msg makeConversationWithAux ao 
    where
        receiverId = if agentIdOut ao == 0 then 1 else 0
        msg =  trace ("makeConversationWith " ++ (show n) ++ " receiverId = " ++ (show receiverId))  (receiverId, Hello n)

        makeConversationWithAux :: ConversationAgentSender
        makeConversationWithAux ao e (Just (senderId, msg@(Hello n)))
            | n > 5 = trace ("Agent " ++ (show $ agentIdOut ao) ++ " receives reply: " ++ (show msg) ++ " but stoppin") (conversationEnd a1, e)
            | otherwise = trace ("Agent " ++ (show $ agentIdOut ao) ++ " receives reply: " ++ (show msg) ++ " continuing") (makeConversationWith (n + 1) ao, e)
            where
                (g, a0) = agentRandomSplit ao

                s = 42

                adef = AgentDef { adId = 100+n,
                            adState = s,
                            adInitMessages = NoEvent,
                            adConversation = Just conversationHandler,
                            adBeh = conversationAgentBehaviour,
                            adRng = g }

                a1 = createAgent adef a0

        makeConversationWithAux ao e _ = trace ("Agent " ++ (show $ agentIdOut ao) ++ " receives Nothing -> stopping") (conversationEnd ao, e)

conversationAgentBehaviour :: ConversationAgentBehaviour
conversationAgentBehaviour = proc (ain, e) ->
    do
        let ao = trace ("conversationAgentBehaviour")  agentOutFromIn ain
        let ao' = makeConversationWith 0 ao
        returnA -< (ao', e)
------------------------------------------------------------------------------------------------------------------------