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
    rng <- newStdGen
    
    let env = createEnvironment
                          Nothing
                          (0,0)
                          moore
                          WrapBoth
                          []
                          rng
                          Nothing
    return (as, env)

randomAgent :: AgentId -> IO ConversationAgentDef
randomAgent agentId = 
  do
    r <- getStdRandom (randomR randomRangeCounter)
    rng <- newStdGen

    let s = r

    return AgentDef { adId = agentId,
                adState = s,
                adEnvPos = (0,0),
                adInitMessages = NoEvent,
                adConversation = Just conversationHandler,
                adBeh = conversationAgentBehaviour,
                adRng = rng }