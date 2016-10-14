module SIRConcurrent where
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.IO
import System.IO.Unsafe
import System.Random
import Debug.Trace

{-
NOTE: STM can be used in deterministic, non parallel way: only atomically introduces IO. this could be a remedy for pulling arround all the mailboxes.

this concurrent implementation is only needed when real continuous time is needed, but in which model is this the case?
-}

data MessageType d = AgentStart | AgentStop | AgentNeighbours | AgentDomain d deriving (Show, Eq)

data Message d = Message
  {
    msgType :: MessageType d,
    msgNeighbours :: Maybe [TChan (Message d)],
    msgContent :: Maybe String
  }
    
data AgentInfrastructure d = AgentInfrastructure
  {
    agentProcess :: ThreadId,
    agentMBox :: TChan (Message d),
    agentMsgQueue :: [(Message d, TChan (Message d))]
  }

type MessageHandler s d = (Message d -> Agent s d -> Agent s d)
type ActivityHandler s d = (Agent s d -> Agent s d)

data Agent s d = Agent
  {
    agentId :: Int,
    agentInfra :: AgentInfrastructure d,
    agentNeighbours :: [TChan (Message d)],
    agentState :: s,
    agentMsgHandler :: MessageHandler s d,
    agentActivityHandler :: ActivityHandler s d
  }

instance Eq (Agent s d) where
  a1 == a2 = (agentId a1) == (agentId a2)

_agentThreadFunc :: Agent s d -> IO ()
_agentThreadFunc aInit = do
  hSetBuffering stdin LineBuffering
  t <- myThreadId
  let is = agentInfra aInit
  let fullAgent = aInit { agentInfra = is { agentProcess = t } }
  --putStrLn $ "Created Agent with " ++ show t ++ ", listening to incoming messages"
  listenToMBox fullAgent
  
createAgent :: Int -> s -> ActivityHandler s d -> MessageHandler s d -> IO (Agent s d)
createAgent id initState actHandler msgHandler = do
  m <- newTChanIO
  let is = AgentInfrastructure { agentMBox = m, agentMsgQueue = [] }
  let a = Agent { agentId = id, agentInfra = is, agentState = initState, agentNeighbours = [], agentMsgHandler = msgHandler, agentActivityHandler = actHandler }
  t <- forkIO $ _agentThreadFunc a
  return a { agentInfra = is { agentProcess = t } }

listenToMBox :: Agent s d -> IO ()
listenToMBox a = do
  let mbox = agentMBox (agentInfra a)
  let actHdl = agentActivityHandler a
  let a' = actHdl a
  ret <- atomically $ tryReadTChan mbox
  matchSTM a' ret
  return ()

matchSTM :: Agent s d -> Maybe (Message d) -> IO ()
matchSTM a Nothing = do
  a' <- processQueuedMessages a
  listenToMBox a'
matchSTM a (Just d) = do
  putStrLn $ "Received message in " ++ show (agentId a)
  let handler = agentMsgHandler a
  let newA = handler d a
  a' <- processQueuedMessages newA
  listenToMBox a'

sendMessage :: Message d -> TChan (Message d) -> IO ()
sendMessage  msg targetBox = do
  atomically $ writeTChan targetBox msg

processQueuedMessages :: Agent s d -> IO (Agent s d)
processQueuedMessages a = do
  let oldAgentInfra = agentInfra a
  let  oldAgentMsgQueue = agentMsgQueue oldAgentInfra
  foldr (\(msg, box) acc -> do
            ret <- sendMessage msg box
            acc) (return a) oldAgentMsgQueue
  return a { agentInfra = oldAgentInfra { agentMsgQueue = [] } }

queueMessage :: Agent s d -> Message d -> TChan (Message d) -> Agent s d
queueMessage a msg receiver = a { agentInfra = oldAgentInfra { agentMsgQueue = newAgentMsgQueue } }
  where
    oldAgentInfra = agentInfra a
    oldAgentMsgQueue = agentMsgQueue oldAgentInfra
    newAgentMsgQueue = oldAgentMsgQueue ++ [(msg, receiver)]

allMailboxes :: [Agent s d] -> [TChan (Message d)]
allMailboxes as = map (\a -> (agentMBox (agentInfra a))) as

randomBool :: Float -> Bool
randomBool p = p >= rand
  where
    rand = unsafePerformIO (getStdRandom (randomR (0.0, 1.0)))

sendToAgents :: Message d -> [Agent s d] -> IO ()
sendToAgents msg as = foldr (\a acc -> sendMessage msg (agentMBox (agentInfra a))) (return ()) as
    
--------------------------------------------------------------------------------------------------------------------
-- SIR-Specific 

populationCount :: Int
populationCount = 10

infectionProb :: Float
infectionProb = 0.5

initInfectProb :: Float
initInfectProb = 0.1

daysInfectous :: Int
daysInfectous = 3

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
data SIRDomain = Contact deriving (Show, Eq)

data SIRAgentState = SIRAgentState
  {
    sirState :: SIRState,
    daysInfected :: Int
  } deriving (Show)

type SIRMessage = Message SIRDomain
type SIRAgent = Agent SIRAgentState SIRDomain

type SIRMessageHandler = SIRMessage -> SIRAgent -> SIRAgent
type SIRActivityHandler = SIRAgent -> SIRAgent

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  as <- populateSIR populationCount
  sendToAgents startMsg as
  sendToAgents (neighboursMsg $ allMailboxes as) as 
  waitMs 5000
  -- in SIR the running of each agent in its separate thread is really a problem as there is then no common sense of the time-flow which is important for this simulation
  return ()

infectionMsg :: SIRMessage
infectionMsg = Message { msgType = AgentDomain Contact, msgNeighbours = Nothing, msgContent = Just "Infected" }

startMsg :: SIRMessage
startMsg = Message { msgType = AgentStart, msgNeighbours = Nothing, msgContent = Nothing }

neighboursMsg :: [TChan (Message SIRDomain)] -> SIRMessage
neighboursMsg ns = Message { msgType = AgentNeighbours, msgNeighbours = (Just ns), msgContent = Nothing }

waitMs :: Int -> IO ()
waitMs ms = threadDelay (1000 * ms)

sirMsgHandler :: SIRMessageHandler
sirMsgHandler msg a
  | t == AgentStart = handleStart a
  | t == AgentStop = handleStop a
  | t == AgentNeighbours = handleNeighbours a ns
  | t == (AgentDomain Contact) = handleContact a c
   where
     t = msgType msg
     c = msgContent msg
     ns = msgNeighbours msg

sirAgentActivity :: SIRActivityHandler
sirAgentActivity a
  | isInfected a = makeRandContact $ cureInfectedAgent a
  | otherwise = a
  
handleStart :: SIRAgent -> SIRAgent
handleStart a = selfInfectRand initInfectProb a

handleStop :: SIRAgent -> SIRAgent
handleStop a = a -- NOTE: nothing to do

handleNeighbours :: SIRAgent -> Maybe [TChan (Message SIRDomain)] -> SIRAgent
handleNeighbours a (Just ns) = a { agentNeighbours = (agentNeighbours a) ++ ns}
handleNeighbours a _ = a -- NOTE: should not occur, as when sending Neighbours-Message then it should contain neighbours in the according list

handleContact :: SIRAgent -> Maybe String -> SIRAgent
handleContact a (Just "Infected") = selfInfectRand infectionProb a
handleContact a _ = a -- NOTE: should not occur, as when sending Contact-Message then it should contain a content-string

cureInfectedAgent :: SIRAgent -> SIRAgent
cureInfectedAgent a
  | hasRecovered = a{ agentState=oldAgentState{sirState=Susceptible,daysInfected=0} }
  | otherwise = a{ agentState=oldAgentState {daysInfected=newDaysInfected} }
  where
    oldAgentState = agentState a
    oldDaysInfected = daysInfected oldAgentState
    newDaysInfected = oldDaysInfected - 1
    hasRecovered = newDaysInfected == 0
    
isInfected :: SIRAgent -> Bool
isInfected a = (sirState (agentState a)) == Infected
            
makeRandContact :: SIRAgent -> SIRAgent
makeRandContact a = queueMessage a infectionMsg neighbour
  where
    neighbour = getRandomNeighbour a

-- TODO: ommit self
-- handle case of no neighbours
getRandomNeighbour :: SIRAgent -> TChan (Message SIRDomain)
getRandomNeighbour a = randNeighbour
  where
    neighbours = agentNeighbours a
    neighboursCount = length neighbours
    randIdx = unsafePerformIO (getStdRandom (randomR (0, neighboursCount-1)))
    randNeighbour = neighbours !! randIdx
    
selfInfectRand :: Float -> SIRAgent -> SIRAgent
selfInfectRand p a
  | doInfection && (not $ isInfected a) = a { agentState=oldAgentState { sirState=Infected, daysInfected=daysInfectous } }
  | otherwise = a
    where
      doInfection = randomBool p
      oldAgentState = agentState a
      
populateSIR :: Int -> IO [SIRAgent]
populateSIR n = foldr (\i acc ->
                        do
                          a <- createSIRAgent i
                          as <- acc
                          return (a:as)) (return []) [1..n]

createSIRAgent :: Int -> IO SIRAgent
createSIRAgent id = createAgent id SIRAgentState{sirState=Susceptible,daysInfected=0} sirAgentActivity sirMsgHandler
