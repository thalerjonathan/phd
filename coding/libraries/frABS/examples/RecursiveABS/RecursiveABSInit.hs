module RecursiveABS.RecursiveABSInit (
    createRecursiveABS
  ) where

import RecursiveABS.RecursiveABSModel

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createRecursiveABS :: Int -> IO ([RecursiveABSAgentDef], RecursiveABSEnvironment)
createRecursiveABS agentCount = 
  do
    as <- mapM randomAgent [0..agentCount-1]
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
    
randomAgent :: Int -> IO RecursiveABSAgentDef
randomAgent agentId = 
  do
    r <- getStdRandom (randomR randomRangeCounter)
    rng <- newStdGen

    let s = r

    return AgentDef { adId = agentId,
                adState = s,
                adEnvPos = (0,0),
                adInitMessages = NoEvent,
                adConversation = Nothing,
                adBeh = recursiveABSAgentBehaviour,
                adRng = rng }