module Conversation.ConversationInit where

import Conversation.ConversationModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random
import System.IO

createConversationAgentsAndEnv :: Int -> IO ([ConversationAgentDef], ConversationEnvironment)
createConversationAgentsAndEnv count = do
                                        as <- mapM randomAgent [0..count-1]

                                        let env = createEnvironment
                                                              Nothing
                                                              (0,0)
                                                              moore
                                                              WrapBoth
                                                              []
                                        return (as, env)
    where
        randomAgent :: AgentId -> IO ConversationAgentDef
        randomAgent agentId = do
                                r <- getStdRandom (randomR randomRangeCounter)
                                rng <- newStdGen

                                let s = ConversationAgentState {
                                    convCounter = r,
                                    convRng = rng
                                }

                                let a = AgentDef { adId = agentId,
                                            adState = s,
                                            adEnvPos = (0,0),
                                            adInitMessages = NoEvent,
                                            adBeh = conversationAgentBehaviour }

                                return a