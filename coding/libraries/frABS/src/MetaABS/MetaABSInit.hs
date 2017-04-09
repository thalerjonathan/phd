module MetaABS.MetaABSInit where

import MetaABS.MetaABSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createMetaABSAgentsAndEnv :: Int -> IO ([MetaABSAgentDef], MetaABSEnvironment)
createMetaABSAgentsAndEnv agentCount = do
                                        as <- mapM randomAgent [0..agentCount-1]

                                        let env = createEnvironment
                                                              Nothing
                                                              (0,0)
                                                              moore
                                                              WrapBoth
                                                              []
                                        return (as, env)
    where
        randomAgent :: Int -> IO MetaABSAgentDef
        randomAgent agentId = do
                                r <- getStdRandom (randomR randomRangeCounter)
                                rng <- newStdGen

                                let s = MetaABSAgentState {
                                    mabsCounter = r,
                                    mabsRng = rng
                                }

                                let a = AgentDef { adId = agentId,
                                            adState = s,
                                            adEnvPos = (0,0),
                                            adInitMessages = NoEvent,
                                            adConversation = Nothing,
                                            adBeh = metaABSAgentBehaviour }

                                return a