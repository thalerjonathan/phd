module Main where
import System.IO
import Control.Monad
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.IO.Unsafe
import System.Random
import System.IO.Unsafe

{- TODO agent-model
- need neighbouring of agents (network)

- pattern match on Agent s: sender
- pattern match on Message m: message

- want to send to single random agent
- want to send to single agent
- want to broadcast to all agents
- want to broadcast to all neighbours

- QUESTION: is it possible to send lambdas with data enclosed in the STD?

- need some kind of environment: is a central agent with whom all can interact: can be queried about other agents, infrastructure,... note: it is NOT another domain-specific agent, which is known to ALL existing agents, this has to be implemented in the domain-specific model if sucha thing is required

- want to have proactivity, but how? e.g. calling regularly a function after having polled the message-box?

- is it possible to use deterministic parallelism instead of classic concurrency e.g. transparent parallel execution and communication of data? would resemble more a pure functional style and allows to reason about programs

3 approaches: 1. no parallelism/concurrency, 2. deterministic parallelism, 3. indeterministic concurrency

what about ignoring parallelism and concurrency for now and just iterate round robin through agents?

experiment with basic forkIO, MVar and STM: two threads communicating. install threadscope and look at execution

instead of function to forkIO: use lambda which accesses data from outside forkio

- but in all cases: use IVar, MVar or STM to "communicate" among agents: need some way of sharing data with each other. question: what would be the alternative?

- WANT TO RETAIN ABILITY TO REASON ABOUT PROGRAMM DESPITE USING PARALLELISM AND CONCURRENCY
- ALL HAS TO BE TYPESAFE (but this is ensured by Haskell anyway)
-}

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

------------------------------------------------------------------------------------------------------
-- TEST
data MsgType d = Start | Stop | Domain d deriving (Show)
data MsgDomain = A | B | C deriving (Show, Eq)

matchMsgTypeNum :: (Num d, Eq d, Show d) => MsgType d -> IO ()
matchMsgTypeNum Start = putStr "matchMsgTypeNum: Start"
matchMsgTypeNum Stop = putStr "matchMsgTypeNum: Stop"
matchMsgTypeNum (Domain x) = putStr ("matchMsgTypeNum: Domain " ++ show x)

matchMsgTypeBool :: (Bool d) => MsgType d -> IO ()
matchMsgTypeBool Start = putStr "matchMsgTypeBool: Start"
matchMsgTypeBool Stop = putStr "matchMsgTypeBool: Stop"
matchMsgTypeBool (Domain True) = putStr ("matchMsgTypeBool: Domain True")
matchMsgTypeBool (Domain False) = putStr ("matchMsgTypeBool: Domain False")

matchMsgTypeDom :: (MsgDomain d) => MsgType d -> IO ()
matchMsgTypeDom Start = putStr "matchMsgTypeDom: Start"
matchMsgTypeDom Stop = putStr "matchMsgTypeDom: Stop"
matchMsgTypeDom (Domain A) = putStr ("matchMsgTypeDom: Domain A")
matchMsgTypeDom (Domain B) = putStr ("matchMsgTypeDom: Domain B")
matchMsgTypeDom (Domain C) = putStr ("matchMsgTypeDom: Domain C")
------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------
-- SEQUENTIAL APPROACH
type AgentId = Int

data MessageType = AgentStart | AgentStop | AgentContent deriving (Show)
data Message p = Message {
  sender :: AgentId,
  receiver :: AgentId,
  msgType :: MessageType,
  content :: Maybe p
} deriving (Show)
              
data Agent s p = Agent
  {
    agentId :: AgentId,
    agentMBox :: [Message p],
    agentState :: s         
  }

createAgent :: Int -> s -> Agent s p
createAgent i initState = Agent { agentId = i, agentMBox = [], agentState = initState }

processAgents :: [Agent s p] -> [Agent s p]
processAgents as = as

emptyMessage :: MessageType -> Message p
emptyMessage t = Message{msgType=t, sender=(-1), receiver=(-1), content=Nothing}

startAgents :: [Agent s p] -> [Agent s p]
startAgents as = map (sendMessage $ emptyMessage AgentStart) as

stopAgents :: [Agent s p] -> [Agent s p]
stopAgents as = map (sendMessage $ emptyMessage AgentStop) as

sendMessage :: Message p -> Agent s p -> Agent s p
sendMessage msg a = a { agentMBox = newMBox }
  where
    mbox = agentMBox a
    newMBox = mbox ++ [msg]

showAgent :: (Show s, Show p) => Agent s p -> String
showAgent a = "Agent " ++ show id ++ " state: " ++ show state ++ ", mbox: " ++ show mbox
  where
    id = agentId a
    state = agentState a
    mbox = agentMBox a

printAgents :: (Show s, Show p) => [Agent s p] -> IO ()
printAgents [] = return ()
printAgents (a:as) = do
  putStr (showAgent a)
  putStr "\n"
  printAgents as

-- SIR-specific stuff -------------------------------------------------------------------------

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

data SIRProtocoll = ContactSusceptible | ContactInfected | ContactRecovered deriving (Show)
data SIRAgentState = SIRAgentState
  {
    sirState :: SIRState,
    daysInfected :: Int
  } deriving (Show)
                     
type SIRAgent = Agent SIRAgentState SIRProtocoll

populationCount :: Int
populationCount = 100

simStepsCount :: Int
simStepsCount = 2

infectionProb :: Float
infectionProb = 0.1

daysInfectous :: Int
daysInfectous = 3

main :: IO()
main = do
  let agents = populateSIR populationCount
  let startedAgents = startAgents agents
  let finalAgents = executeSimSteps simStepsCount startedAgents
  printAgents (head finalAgents)
  
executeSimSteps :: Int -> [SIRAgent] -> [[SIRAgent]]
executeSimSteps n initAs = foldr (\i acc -> simStep (head acc) : acc) [initAs] [1..n]

simStep :: [SIRAgent] -> [SIRAgent]
simStep as = map updateAgent $ map processMessages as

{-
randomContacts :: [SIRAgent] -> [SIRAgent]
randomContacts as = map ()
  where
    as = [a]
    agentCount = length as
    randIdx = unsafePerformIO (getStdRandom (randomR (0, agentCount-1)))
    randAgent = as !! randIdx
    msg = Message{msgType=AgentContent, sender=(-1), receiver=(-1), content=msgContent}
    msgContent = if infectionState==Infected then (Just ContactInfected) else Nothing
    infectionState = sirState $ agentState a
-}

contactRandomAgent :: SIRAgent -> SIRAgent
contactRandomAgent a = sendMessage msg randAgent
  where
    as = [a]
    agentCount = length as
    randIdx = unsafePerformIO (getStdRandom (randomR (0, agentCount-1)))
    randAgent = as !! randIdx
    msg = Message{msgType=AgentContent, sender=(-1), receiver=(-1), content=msgContent}
    msgContent = if infectionState==Infected then (Just ContactInfected) else Nothing
    infectionState = sirState $ agentState a
    
processMessages :: SIRAgent -> SIRAgent
processMessages initA = agentClearMBox
  where
    messages = agentMBox initA
    agentAfterProc = foldr (\msg a -> matchMessageType msg a) initA messages
    agentClearMBox = agentAfterProc{agentMBox=[]} 

updateAgent :: SIRAgent -> SIRAgent
updateAgent a = contactRandomAgent $ recoverAgent a
      
recoverAgent :: SIRAgent -> SIRAgent
recoverAgent a
  | infectionState == Infected = if (remainingDays - 1) == 0 then a {agentState=SIRAgentState{sirState=Recovered, daysInfected=0}} else a {agentState=SIRAgentState{sirState=Infected, daysInfected=remainingDays-1}}
  | otherwise = a
  where
    as = agentState a
    infectionState = sirState as
    remainingDays = daysInfected as

matchMessageType :: Message SIRProtocoll -> SIRAgent -> SIRAgent
matchMessageType (Message {msgType=AgentContent, content=c}) a = messageReceived c a
matchMessageType Message {msgType=AgentStart} a = agentStarted a
matchMessageType Message {msgType=AgentStop} a = agentStoped a

messageReceived :: Maybe SIRProtocoll -> SIRAgent -> SIRAgent
messageReceived (Just ContactSusceptible) a = a
messageReceived (Just ContactInfected) a = contactWithInfected a
messageReceived (Just ContactRecovered) a = a
messageReceived Nothing a = a

contactWithInfected :: SIRAgent -> SIRAgent
contactWithInfected a
  | oldSirState == Susceptible = if randInfection then a {agentState=SIRAgentState{sirState=Infected, daysInfected=daysInfectous}} else a
  | otherwise = a                                                                                                           
  where
    oldAgentState = agentState a
    oldSirState = sirState $ agentState a
    randInfection = randomBool infectionProb

agentStarted :: SIRAgent -> SIRAgent
agentStarted a = a

agentStoped :: SIRAgent -> SIRAgent
agentStoped a = a

createSIRAgent :: Int -> SIRAgent
createSIRAgent i = createAgent i randState
  where
     randInfection = randomBool infectionProb
     randState = if randInfection then SIRAgentState{sirState=Infected, daysInfected=daysInfectous} else SIRAgentState{sirState=Susceptible, daysInfected=0}
                                               
populateSIR :: Int -> [SIRAgent]
populateSIR n = foldr (\i acc -> (createSIRAgent i) : acc) [] [1..n]

randomBool :: Float -> Bool
randomBool p = p >= rand
  where
    rand = unsafePerformIO (getStdRandom (randomR (0.0, 1.0))) 
------------------------------------------------------------------------------------------------------
