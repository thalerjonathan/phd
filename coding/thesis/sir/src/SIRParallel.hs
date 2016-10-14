module SIRParallel where
 
import System.IO
import System.IO.Unsafe
import System.Random
import Debug.Trace

data MessageType d = AgentStart | AgentStop | AgentNeighbours | AgentDomain d deriving (Show, Eq)

type AgentID = Int

data Message d = Message
  {
    msgType :: MessageType d,
    msgNeighbours :: Maybe [AgentID],
    msgSender :: AgentID,
    msgReceiver :: AgentID,
    msgContent :: Maybe String
  } deriving (Show)

type MessageHandler s d = (Message d -> Agent s d -> Agent s d)
type ActivityHandler s d = (Agent s d -> Agent s d)

data Agent s d = Agent
  {
    agentId :: AgentID,
    agentInBox :: [Message d],
    agentOutBox :: [Message d],
    agentNeighbours :: [AgentID],
    agentState :: s,
    agentMsgHandler :: MessageHandler s d,
    agentActivityHandler :: ActivityHandler s d
  }
 
showAgent :: (Show s, Show p) => Agent s p -> String
showAgent a = "Agent " ++ show id ++ " state: " ++ show state ++ ", mbox: " ++ show mbox
  where
    id = agentId a
    state = agentState a
    mbox = agentInBox a
    
printAgents :: (Show s, Show p) => [Agent s p] -> IO ()
printAgents [] = return ()
printAgents (a:as) = do
  putStr (showAgent a)
  putStr "\n"
  printAgents as
  
instance Eq (Agent s d) where
  a1 == a2 = (agentId a1) == (agentId a2)
  
createAgent :: Int -> s -> ActivityHandler s d -> MessageHandler s d -> Agent s d
createAgent id initState actHandler msgHandler = Agent { agentId = id, agentInBox = [], agentOutBox = [], agentNeighbours = [], agentState = initState, agentMsgHandler = msgHandler, agentActivityHandler = actHandler }

deliverMsg :: Message d -> Agent s d ->  Agent s d
deliverMsg msg a = a { agentInBox = newInBox }
  where
    oldInBox = agentInBox a
    newInBox = oldInBox ++ [msg]
    
queueMsg :: Message d -> Agent s d -> Agent s d
queueMsg msg a = a { agentOutBox = newOutBox }
  where
    oldOutBox = agentOutBox a
    newOutBox = oldOutBox ++ [msg]

processMsg :: Message d -> Agent s d -> Agent s d
processMsg msg a = handler msg a
  where
    handler = agentMsgHandler a

queueMsgToAll :: Message d -> [Agent s d] -> [Agent s d]
queueMsgToAll msg as = map (\a -> queueMsg msg a ) as

type SimStepClbk s d = (Int -> [Agent s d] -> Bool)

runSimulation :: Int -> [Agent s d] -> SimStepClbk s d -> [[Agent s d]]
runSimulation 0 as stepClbk = []
runSimulation steps as stepClbk = runSimulation steps as stepClbk

runSimulation' t as stepClbk = if cont then (runSimulation (t-1) as stepClbk) ++ [as'] else []
  where
    as' = simStep as
    cont = stepClbk t as

simStep :: [Agent s d] -> [Agent s d] 
simStep as = map (\a -> agentSimStep a) $ processOutMessages as -- TODO: implement data-parallelism here with Par-Monad and IVars

processOutMessages :: [Agent s d] -> [Agent s d]
processOutMessages as = as''
  where
    as' = map (\a -> foldr (\msg a' -> deliverMsg msg a) a (gatherMessagesFor a as) ) as
    as'' = map (\a -> a { agentOutBox = [] } ) as'

gatherMessagesFor :: Agent s d -> [Agent s d] -> [Message d]
gatherMessagesFor a as = foldr (\a' acc -> (messagesFor a a') ++ acc) [] as
  where
    receiverId = agentId a

messagesFor :: Agent s d -> Agent s d -> [Message d]
messagesFor a a' = foldr (\msg acc -> if (receiverId == msgReceiver msg) then acc ++ [msg] else acc ) [] msgs
  where
    receiverId = agentId a
    msgs = agentOutBox a'

agentSimStep :: Agent s d -> Agent s d
agentSimStep a = processAllInMsg $ actHdl a 
  where
    actHdl = agentActivityHandler a

processAllInMsg :: Agent s d -> Agent s d
processAllInMsg a = a''
  where
    inMsgs = agentInBox a
    a' = foldr (\msg acc -> processMsg msg a ) a inMsgs
    a'' = a { agentInBox = [] }

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

simSteps :: Int
simSteps = 1000

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
data SIRDomain = Contact deriving (Show, Eq)

data SIRAgentState = SIRAgentState
  {
    sirState :: SIRState,
    daysInfected :: Int
  } deriving (Show)

type SIRMessage = Message SIRDomain
type SIRAgent = Agent SIRAgentState SIRDomain

type SIRMessageHandler = (SIRMessage -> SIRAgent -> SIRAgent)
type SIRActivityHandler = (SIRAgent -> SIRAgent)
type SIRStepClbk = (Int -> [SIRAgent] -> Bool)

{-
main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  as <- populateSIR populationCount
  queueMsgToAll startMsg as
  queueMsgToAll (neighboursMsg $ allIds as) as 
  allAgents <- runSimulation simSteps as sirStepClbk
  return ()
-}

testSIR :: [[SIRAgent]]
testSIR = runSimulation simSteps neighboursPop sirStepClbk
  where
    population = populateSIR populationCount
    startedPop = queueMsgToAll startMsg population
    neighboursPop = queueMsgToAll (neighboursMsg $ allIds startedPop) startedPop 

allIds :: [Agent s d] -> [AgentID]
allIds as = map (\a -> agentId a ) as

sirStepClbk :: SIRStepClbk
sirStepClbk t as = not $ isAnyInfected as

isAnyInfected :: [SIRAgent] -> Bool
isAnyInfected as = any (\a -> isInfected a) as

infectionMsg :: AgentID -> SIRMessage
infectionMsg receiver = Message { msgType = AgentDomain Contact, msgNeighbours = Nothing, msgReceiver = receiver, msgSender = -1, msgContent = Just "Infected" }

startMsg :: SIRMessage
startMsg = Message { msgType = AgentStart, msgNeighbours = Nothing, msgReceiver = -1, msgSender = -1, msgContent = Nothing }

neighboursMsg :: [AgentID] -> SIRMessage
neighboursMsg ns = Message { msgType = AgentNeighbours, msgReceiver = -1, msgSender = -1, msgNeighbours = (Just ns), msgContent = Nothing }

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

handleNeighbours :: SIRAgent -> Maybe [AgentID] -> SIRAgent
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
makeRandContact a = queueMsg (infectionMsg neighbour) a
  where
    neighbour = getRandomNeighbour a

getRandomNeighbour :: SIRAgent -> AgentID
getRandomNeighbour a = if ( randNeighbour == (agentId a) ) then getRandomNeighbour a else randIdx
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
      
populateSIR :: Int -> [SIRAgent]
populateSIR n = foldr (\i acc -> acc ++ [(createSIRAgent i)]) [] [1..n]

createSIRAgent :: Int -> SIRAgent
createSIRAgent id = createAgent id SIRAgentState{sirState=Susceptible,daysInfected=0} sirAgentActivity sirMsgHandler

randomBool :: Float -> Bool
randomBool p = p >= rand
  where
    rand = unsafePerformIO (getStdRandom (randomR (0.0, 1.0)))
