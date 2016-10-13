module SIRConcurrent where

------------------------------------------------------------------------------------------------------
-- CONCURRENCY-APPROACH
{-
data MessageType d = AgentStart | AgentStop | AgentNeighbours | AgentDomain d

data AgentInfrastructure d = AgentInfrastructure
  {
    agentProcess :: ThreadId,
    agentMBox :: TChan (MessageType d)
  }
                           
data Agent s d = Agent
  {
    agentInfra :: AgentInfrastructure d,
    agentNeighbours :: [AgentInfrastructure d],
    agentEnvironment :: AgentInfrastructure d,
    agentState :: s            
  }

_agentThreadFunc :: Agent s d -> IO ()
_agentThreadFunc aInit = do
  t <- myThreadId
  let is = agentInfra aInit
  let a = aInit { agentInfra = is { agentProcess = t } }
  putStrLn $ "Created Agent with " ++ show t
  --forever $ atomically (readTChan $ agentMBox a) >>= f
  --return ()
  
createAgent :: s -> IO (Agent s d)
createAgent initState = do
  m <- newTChanIO
  let is = AgentInfrastructure { agentMBox = m }
  let a = Agent { agentInfra = is,  agentState = initState, agentNeighbours = [] }
  t <- forkIO $ _agentThreadFunc a
  return a { agentInfra = is { agentProcess = t } }
-}
------------------------------------------------------------------------------------------------------
