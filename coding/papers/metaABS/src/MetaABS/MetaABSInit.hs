module MetaABS.MetaABSInit where

import MetaABS.MetaABSModel

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random
import System.IO

createMetaABSAgentsAndEnv :: Int -> IO ([MetaABSAgentDef], MetaABSEnvironment)
createMetaABSAgentsAndEnv count = do
                                        as <- mapM randomAgent [0..count-1]

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
                                            adBeh = metaABSAgentBehaviour }

                                return a