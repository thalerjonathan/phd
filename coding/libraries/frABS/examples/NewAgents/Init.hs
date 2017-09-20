module NewAgents.Init (
    initNewAgents
  ) where

import NewAgents.Model 
import NewAgents.Agent

import FRP.FrABS

import FRP.Yampa

import System.Random
import Control.Monad.Random

initNewAgents :: Int -> IO ([NewAgentDef], NewAgentEnvironment)
initNewAgents n = 
  do
    adefs <- evalRandIO $ mapM createNewAgent [0..(n-1)]
    return (adefs, ())

createNewAgent :: AgentId -> Rand StdGen NewAgentDef
createNewAgent aid = 
    do
        rng <- getSplit
        randValue <- getRandomR (0, 43)

        let s = NewAgentState {
          value = randValue
        }

        let adef = AgentDef {
           adId = aid,
           adState = s,
           adConversation = Nothing,
           adInitMessages = NoEvent,
           adBeh = (newAgentBehaviour s),
           adRng = rng 
        }

        return adef