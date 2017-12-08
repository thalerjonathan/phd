{-# LANGUAGE Arrows #-}
module Main where

import System.IO

import Control.Monad.Random
import Control.Concurrent.STM
import FRP.BearRiver

type AgentId = Int
type DataFlow d = (AgentId, d)

data AgentIn d = AgentIn
  {
    aiId    :: AgentId
  , aiData  :: [DataFlow d]
  } deriving (Show)

data AgentOut o d = AgentOut
  {
    aoData        :: [DataFlow d]
  , aoObservable  :: Maybe o
  } deriving (Show)

type Agent m o d        = SF m (AgentIn d) (AgentOut o d)

type STMTestObservable  = Double
data STMTestMsg         = DataFlow Int deriving (Show, Eq)
type STMTestAgentIn     = AgentIn STMTestMsg
type STMTestAgentOut    = AgentOut STMTestObservable STMTestMsg
type STMTestAgent g     = Agent (RandT g STM) STMTestObservable STMTestMsg

type STMTestEnvCellData = (Bool, Double)
type STMTestEnv         = [TMVar STMTestEnvCellData]

agentCount :: Int
agentCount = 10

cellCount :: Int
cellCount = 15

rngSeed :: Int
rngSeed = 42

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let _g = mkStdGen rngSeed
  
  _env <- atomically $ initTestEnv cellCount

  print "Hello"

initTestEnv :: Int -> STM STMTestEnv
initTestEnv n = do
  let cellData = replicate n ((False, 0) :: STMTestEnvCellData)
  cells <- mapM newTMVar cellData
  return cells

initTestAgent :: STMTestEnv -> Rand g (STMTestAgent g)
initTestAgent env = do
  

testAgent :: RandomGen g 
          => Double
          -> STMTestEnv
          -> STMTestAgent g
testAgent r0 env = proc ain -> do
  returnA -< agentOutObs r0

testEnvironment :: RandomGen g 
                => STMTestEnv
                -> STMTestAgent g
testEnvironment env = undefined

-------------------------------------------------------------------------------
agentId :: AgentIn d -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut o d -> Maybe o
agentObservable AgentOut { aoObservable = os } = os

agentIn :: AgentId -> AgentIn d
agentIn aid = AgentIn {
    aiId    = aid
  , aiData  = []
  }

agentOut :: AgentOut o d
agentOut = AgentOut {
    aoData        = []
  , aoObservable  = Nothing
  }

agentOutObs :: o -> AgentOut o d
agentOutObs o = AgentOut {
    aoData        = []
  , aoObservable  = Just o
  }
