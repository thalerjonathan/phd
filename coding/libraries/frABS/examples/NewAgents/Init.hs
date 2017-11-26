module Init 
  (
    initNewAgents
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import Model 
import Agent

initNewAgents :: Int -> IO ([NewAgentDef], NewAgentEnvironment)
initNewAgents n = do
  adefs <- evalRandIO $ mapM createNewAgent [0..(n-1)]
  return (adefs, ())

createNewAgent :: AgentId -> Rand StdGen NewAgentDef
createNewAgent aid = do
  --randValue <- getRandomR (0, 43)

  let s = 1

  let adef = AgentDef {
    adId = aid
  , adInitMessages = NoEvent
  , adBeh = newAgentBehaviour s
  }

  return adef