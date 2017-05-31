module MessageSampling.MessageSamplingInit where

import MessageSampling.MessageSamplingModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createMessageSamplingAgentsAndEnv :: Int -> IO ([MessageSamplingAgentDef], MessageSamplingEnvironment)
createMessageSamplingAgentsAndEnv count = do
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
    where
        randomAgent :: AgentId -> IO MessageSamplingAgentDef
        randomAgent agentId = do
                                r <- getStdRandom (randomR randomRangeCounter)
                                rng <- newStdGen

                                let s = MessageSamplingState {
                                    msgSampCounter = r
                                }

                                let a = AgentDef { adId = agentId,
                                            adState = s,
                                            adEnvPos = (0,0),
                                            adInitMessages = NoEvent,
                                            adConversation = Nothing,
                                            adBeh = messageSamplingAgentBehaviour,
                                            adRng = rng }

                                return a