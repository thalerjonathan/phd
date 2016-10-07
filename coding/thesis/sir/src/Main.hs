module Main where
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

{- TODO agent-model
- need a mechanism to receive / send generic messages: define protocoll
- need a mechanism to "connect" two actors: make the id of the actors known to each other through a specific message
- need neighbouring of agents (network)
- pattern match on Agent s: sender
- pattern match on Message m: message
- want to send to single random agent
- want to send to single agent
- want to broadcast to all agents
- want to broadcast to all neighbours
-}

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

------------------------------------------------------------------------------------------------------
-- IMPLEMENTATION OF THE SIR-MODEL
data SIRStates = Susceptible | Infected | Recovered
data SIRDomain = Infection
type SIRAgent = Agent SIRStates SIRDomain

populationCount :: Int
populationCount = 1

main :: IO()
main = do
  let agents = populateSIR populationCount
  return ()

populateSIR :: Int -> IO [SIRAgent]
populateSIR 0 = return []
populateSIR n = createAgent Susceptible >>= (\a -> populateSIR (n-1) >>= (\as -> return (a:as)))
------------------------------------------------------------------------------------------------------
