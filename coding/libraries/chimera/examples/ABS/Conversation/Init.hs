module Conversation.Init (
    createConversation
  ) where

import Conversation.Model

import FRP.Yampa

import FRP.FrABS

import System.Random

createConversation :: Int -> IO ([ConversationAgentDef], ConversationEnvironment)
createConversation count =
  do
    as <- mapM randomAgent [0..count-1]
    return (as, ())

randomAgent :: AgentId -> IO ConversationAgentDef
randomAgent agentId = 
  do
    r <- randomRIO randomRangeCounter
    rng <- newStdGen

    return AgentDef { adId = agentId,
                      adState = r,
                      adInitMessages = NoEvent,
                      adConversation = Just conversationHandler,
                      adBeh = conversationAgentBehaviour,
                      adRng = rng }