module RecursiveABS.Init (
    createRecursiveABS
  ) where

import RecursiveABS.Model

import FRP.Yampa

import FRP.FrABS

import System.Random

createRecursiveABS :: Int -> IO ([RecursiveABSAgentDef], RecursiveABSEnvironment)
createRecursiveABS agentCount = 
  do
    as <- mapM randomAgent [0..agentCount-1]
    let e = 0
    
    return (as, e)
    
randomAgent :: Int -> IO RecursiveABSAgentDef
randomAgent agentId = 
  do
    r <- getStdRandom (randomR randomRangeCounter)
    rng <- newStdGen

    return AgentDef { adId = agentId,
                      adState = r,
                      adInitMessages = NoEvent,
                      adConversation = Nothing,
                      adBeh = recursiveABSAgentBehaviour,
                      adRng = rng }