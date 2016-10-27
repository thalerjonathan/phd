module SIRClassic where
 
import System.IO
import System.IO.Unsafe
import System.Random
import Debug.Trace
import Text.JSON
import Data.Time.Clock.POSIX
import Data.Ratio

import Test.HUnit

data MessageType d = AgentStart | AgentStop | AgentNeighbours | AgentDomain d deriving (Show, Eq)

type AgentID = Int

data Message d = Message
  {
    msgType :: MessageType d,
    msgNeighbours :: Maybe [AgentID],
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
    agentActivityHandler :: ActivityHandler s d,
    agentRng :: StdGen
  }
 
showAgent :: (Show s, Show p) => Agent s p -> String
showAgent a = "Agent " ++ show id ++ " state: " ++ show state -- ++ ", in-box: " ++ show inbox ++ ", out-box: " ++ show outbox ++ ", neighbours = " ++ show neighbours
  where
    id = agentId a
    state = agentState a
    inbox = agentInBox a
    outbox = agentOutBox a
    neighbours = agentNeighbours a
    
printAgents :: (Show s, Show p) => [Agent s p] -> IO ()
printAgents [] = return ()
printAgents (a:as) = do
  putStr (showAgent a)
  putStr "\n"
  printAgents as
  
instance Eq (Agent s d) where
  a1 == a2 = (agentId a1) == (agentId a2)
  
createAgent :: Int -> s -> ActivityHandler s d -> MessageHandler s d -> Agent s d
createAgent id initState actHandler msgHandler = Agent { agentId = id, agentInBox = [], agentOutBox = [], agentNeighbours = [], agentState = initState, agentMsgHandler = msgHandler, agentActivityHandler = actHandler, agentRng = rng }
  where
    seed = getUniqueSeed id
    --rng = trace ("Seed: " ++ show seed) (mkStdGen seed)
    rng = mkStdGen seed
    
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
queueMsgToAll msg as = map (\a -> queueMsg (fillMessageReceiver msg a) a) as

fillMessageReceiver :: Message d -> Agent s d -> Message d
fillMessageReceiver msg a = msg'
  where
    msg' = msg { msgReceiver = agentId a }

type SimStepClbk s d = (Int -> [Agent s d] -> Bool)

simStep :: [Agent s d] -> [Agent s d] 
simStep as = map (\a -> agentSimStep a) $ processOutMessages as -- TODO: implement data-parallelism here with Par-Monad and IVars

processOutMessages :: [Agent s d] -> [Agent s d]
processOutMessages as = as''
  where
    as' = map (\a -> foldr (\msg a' -> deliverMsg msg a') a (gatherMessagesFor a as) ) as
    as'' = map (\a -> a { agentOutBox = [] } ) as'

gatherMessagesFor :: Agent s d -> [Agent s d] -> [Message d]
gatherMessagesFor a as = foldr (\a' acc -> acc ++ (messagesFor a a')) [] as
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
    a' = foldr (\msg acc -> processMsg msg acc) a inMsgs
    a'' = a' { agentInBox = [] }

getUniqueSeed :: Int -> Int
getUniqueSeed offset = (offset + fromIntegral (unsafePerformIO timeInMicros))
    
timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

-- RNG-Stuff
randBoolFromAgent :: Agent s d -> Float -> (Bool, Agent s d)
randBoolFromAgent a p = (rb, a')
  where
    rng = agentRng a
    (rb, rng') = randBool rng p
    a' = a { agentRng = rng' }

randIntFromAgent :: Agent s d -> (Int, Int) -> (Int, Agent s d)
randIntFromAgent a ip = (ri, a')
  where
    rng = agentRng a
    (ri, rng') = randomR ip rng
    a' = a { agentRng = rng' }
    
-- PURE functional RNG bool
randBool :: RandomGen g => g -> Float -> (Bool, g)
randBool rng p = (rndBool, rng')
  where
    (rndFloat, rng') = randomR (0.0, 1.0) rng
    rndBool = p >= rndFloat

--------------------------------------------------------------------------------------------------------------------
-- SIR-Specific 

filePath :: String
filePath = "visualization/dynamics.json"

-- the ratio of neighbours the infected agents contacts
infectionRate :: Float
infectionRate = 0.05

-- the probability of getting infected when having contact with an infected agent
infectionProb :: Float
infectionProb = 0.5

-- the probability of becoming infected initially in the simulation
initInfectProb :: Float
initInfectProb = 0.1

-- the number of days an agent stays infected
daysInfected :: Int
daysInfected = 30

-- the number of days an agent stays immune
daysImmune :: Int
daysImmune = 2

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
data SIRDomain = Contact deriving (Show, Eq)

data SIRAgentState = SIRAgentState
  {
    sirState :: SIRState,
    daysInState :: Int
  } deriving (Show)

type SIRMessage = Message SIRDomain
type SIRAgent = Agent SIRAgentState SIRDomain

type SIRMessageHandler = (SIRMessage -> SIRAgent -> SIRAgent)
type SIRActivityHandler = (SIRAgent -> SIRAgent)
type SIRStepClbk = (Int -> [SIRAgent] -> Bool)

-- MAIN-LOOP
runSimulation :: Int -> Int -> Int -> IO ()
runSimulation  popCount replCount maxSteps = do
  repls <- runReplications popCount replCount maxSteps []
  let dyns = combineDynamics repls
  exportSIRDynamics dyns
  putStrLn ("Finished")
  return ()

runReplications :: Int -> Int -> Int -> [[(Int, Int, Int)]] -> IO [[(Int, Int, Int)]]
runReplications popCount 0 maxSteps repls = return repls
runReplications popCount n maxSteps repls = do
  putStrLn ("Replications remaining " ++ show n ++ "...")
  repl <- replication popCount n maxSteps
  putStrLn ""
  let repls' = repls ++ [repl]
  runReplications popCount (n-1) maxSteps repls'

replication :: Int -> Int -> Int -> IO [(Int, Int, Int)]
replication popCount idx maxSteps = do
  let initAgents = initSir popCount idx
  putStr "Simulation steps ... "
  ass <- sirStep 0 maxSteps [initAgents]
  let dyns = simDynamics ass
  return dyns
  
sirStep :: Int -> Int -> [[SIRAgent]] -> IO [[SIRAgent]]
sirStep n maxSteps ass
  | n == maxSteps = do
      putStr $ (show n ++ " ")
      return ass
  | otherwise = do
      --putStr $ (show n ++ " ")
      let as = last ass
      let as' = simStep as
      let cont = sirStepClbk n as'
      let ass' = ass ++ [as']
      if cont == True then
        sirStep (n+1) maxSteps ass'
        else do
          putStr $ (show n ++ " ")
          return ass'

-- EXPORTING DYNAMICS
exportSIRDynamics :: [(Float, Float, Float)] -> IO ()
exportSIRDynamics d = do
  h <- openFile filePath WriteMode
  hPutStr h (encode d)
  hClose h
  return ()
--

combineDynamics :: [[(Int, Int, Int)]] -> [(Float, Float, Float)]
combineDynamics rss = floatDyns
  where
    replCount = fromIntegral $ length rss
    intDyns = foldr (\dyn acc -> sumDynamic acc dyn ) (head rss) (tail rss) 
    floatDyns = map (\(s,i,r) -> ( fromIntegral s / replCount, fromIntegral i / replCount, fromIntegral r / replCount )) intDyns

sumDynamic :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
sumDynamic as bs = map (\((s,i,r), (s',i',r')) -> (s+s', i+i', r+r') ) zipedDyns
  where
    zipedDyns = zip as bs -- replace with sirZip when don't want to be truncated by short runs (but then the dynamics don't add up anymore at the tail, which looks strange and is counter-intuitive!)
      
{- this zip includes the longer list instead of truncating to the short
sirZip :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [((Int, Int, Int), (Int, Int, Int))]
sirZip [] [] = []
sirZip [] (b:bs) = ((0,0,0), b) : sirZip [] bs
sirZip (a:as) []  = (a, (0,0,0)) : sirZip as []
sirZip (a:as) (b:bs) = (a,b) : sirZip as bs
-}

simDynamics :: [[SIRAgent]] -> [(Int, Int, Int)]
simDynamics ass = map (\as -> simStepToDynamic as ) ass

simStepToDynamic :: [SIRAgent] -> (Int, Int, Int)
simStepToDynamic as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = length $ filter isSusceptible as
    infectedCount = length $ filter isInfected as
    recoveredCount = length $ filter isRecovered as

initSir :: Int -> Int -> [SIRAgent]
initSir populationCount replIdx = neighboursPop
  where
    population = populateSIR replIdx populationCount
    startedPop = queueMsgToAll startMsg population
    neighboursPop = queueMsgToAll (neighboursMsg $ allIds startedPop) startedPop 

populateSIR :: Int -> Int -> [SIRAgent]
populateSIR replIdx n = foldr (\i acc -> (createSIRAgent i) : acc ) [] [idxStart..idxEnd]
  where
    idxStart = n * (replIdx - 1) + 1
    idxEnd =   n * replIdx

createSIRAgent :: Int -> SIRAgent
createSIRAgent id = createAgent id SIRAgentState { sirState = Susceptible, daysInState = 0 } sirAgentActivity sirMsgHandler

allIds :: [Agent s d] -> [AgentID]
allIds as = map (\a -> agentId a ) as

sirStepClbk :: SIRStepClbk
sirStepClbk t as = isAnyInfected as

isAnyInfected :: [SIRAgent] -> Bool
isAnyInfected as = any (\a -> isInfected a) as

isNoneInfected :: [SIRAgent] -> Bool 
isNoneInfected as = not $ isAnyInfected as

infectionMsg :: AgentID -> SIRMessage
infectionMsg receiver = Message { msgType = AgentDomain Contact, msgNeighbours = Nothing, msgReceiver = receiver, msgContent = Just "Infected" }

startMsg :: SIRMessage
startMsg = Message { msgType = AgentStart, msgNeighbours = Nothing, msgReceiver = -1, msgContent = Nothing }

neighboursMsg :: [AgentID] -> SIRMessage
neighboursMsg ns = Message { msgType = AgentNeighbours, msgReceiver = -1, msgNeighbours = (Just ns), msgContent = Nothing }

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
  | isInfected a = makeContacts $ cureInfectedAgent a
  | isRecovered a = deimmunizeAgent a
  | otherwise = a

deimmunizeAgent :: SIRAgent -> SIRAgent
deimmunizeAgent a
  | stillImmune = a { agentState = oldAgentState { daysInState = newDaysImmune } }
  | otherwise = a { agentState = oldAgentState { sirState = Susceptible, daysInState = 0 } }
  where
    oldAgentState = agentState a
    oldDaysImmune = daysInState oldAgentState
    newDaysImmune = oldDaysImmune - 1
    stillImmune = newDaysImmune > 0

handleStart :: SIRAgent -> SIRAgent
handleStart a = selfInfectRand initInfectProb a

handleStop :: SIRAgent -> SIRAgent
handleStop a = a -- NOTE: nothing to do

handleNeighbours :: SIRAgent -> Maybe [AgentID] -> SIRAgent
handleNeighbours a (Just ns) =  a { agentNeighbours = (agentNeighbours a) ++ ns}
handleNeighbours a _ = a -- NOTE: should not occur, as when sending Neighbours-Message then it should contain neighbours in the according list

handleContact :: SIRAgent -> Maybe String -> SIRAgent
handleContact a (Just "Infected") = selfInfectRand infectionProb a
handleContact a _ = a -- NOTE: should not occur, as when sending Contact-Message then it should contain a content-string

cureInfectedAgent :: SIRAgent -> SIRAgent
cureInfectedAgent a
  | hasRecovered = a { agentState = oldAgentState { sirState = Recovered, daysInState = daysImmune } }
  | otherwise = a { agentState = oldAgentState { daysInState = newDaysInfected } }
  where
    oldAgentState = agentState a
    oldDaysInfected = daysInState oldAgentState
    newDaysInfected = oldDaysInfected - 1
    hasRecovered = newDaysInfected == 0
    
isSusceptible :: SIRAgent -> Bool
isSusceptible a = (sirState (agentState a)) == Susceptible

isInfected :: SIRAgent -> Bool
isInfected a = (sirState (agentState a)) == Infected

isRecovered :: SIRAgent -> Bool
isRecovered a = (sirState (agentState a)) == Recovered

makeContacts :: SIRAgent -> SIRAgent
makeContacts a = foldr (\i a' -> makeRandContact a') a [1..numContacts]
  where
    agentsNeighbourCount = length $ agentNeighbours a
    numContacts = floor ((fromIntegral agentsNeighbourCount) * infectionRate)

makeRandContact :: SIRAgent -> SIRAgent
makeRandContact a = queueMsg (infectionMsg neighbour) a'
  where
    (neighbour, a') = getRandomNeighbour a

getRandomNeighbour :: SIRAgent -> (AgentID, SIRAgent)
getRandomNeighbour a = if ( randNeighbour == (agentId a) ) then getRandomNeighbour a' else (randNeighbour, a')
  where
    neighbours = agentNeighbours a
    neighboursCount = length neighbours
    (randIdx, a') = randIntFromAgent a (0, neighboursCount-1)
    randNeighbour = neighbours !! randIdx

selfInfectRand :: Float -> SIRAgent -> SIRAgent
selfInfectRand p a 
  | doInfection && susceptible = a' { agentState = oldAgentState { sirState = Infected, daysInState = daysInfected } }
  | otherwise = a
    where
      susceptible = isSusceptible a
      (doInfection, a') = randBoolFromAgent a p
      oldAgentState = agentState a'

--------------------------------------------------------------------------------------------------------------------
-- UNIT TESTS 

-- need a deterministic RNG: starts with 1 and in each next-call returns next natural int up until maxBound :: Int
data DeterministicGen = DeterministicGen Int

instance RandomGen DeterministicGen where
  next = detNext
  genRange = detRange
  split = detSplit

detNext :: DeterministicGen -> (Int, DeterministicGen)
detNext (DeterministicGen n) = (n, DeterministicGen (n+1))

detRange :: DeterministicGen -> (Int, Int)
detRange g = (1, maxBound :: Int)

detSplit :: DeterministicGen -> (DeterministicGen, DeterministicGen)
detSplit g = (g, g)

mkDetGen :: DeterministicGen
mkDetGen = DeterministicGen 1

genDetRands :: (RandomGen g) => Int -> g -> (Int, Int) -> ([Int], g)
genDetRands 0 rng ip = ([], rng)
genDetRands n rng ip = (x : xs, rng'')
  where
    (xs, rng'') = genDetRands (n-1) rng' ip
    (x, rng') = testRand rng ip
    
testRand :: (RandomGen g) => g -> (Int, Int) -> (Int, g)
testRand rng ip = (ri, rng')
  where
    (ri, rng') = randomR ip rng
-------------------------------------------------

testDeimmunization = TestCase ( do
                                let a = Agent { agentState = SIRAgentState{ sirState = Recovered, daysInState = 2 } }
                                let a' = deimmunizeAgent a
                                assertEqual "agent still recovered" (sirState (agentState a')) Recovered
                                assertEqual "agent one day less immune" (daysInState (agentState a')) 1
                                let a'' = deimmunizeAgent a'
                                assertEqual "agent no more immune" (sirState (agentState a'')) Susceptible
                                assertEqual "agent susceptible with 0 days" (daysInState (agentState a'')) 0 )

testCuring = TestCase ( do
                        let a = Agent { agentState = SIRAgentState{ sirState = Infected, daysInState = 2 } }
                        let a' = cureInfectedAgent a
                        assertEqual "agent still infected" (sirState (agentState a')) Infected
                        assertEqual "agent one day less infected" (daysInState (agentState a')) 1
                        let a'' = cureInfectedAgent a'
                        assertEqual "agent has recovered" (sirState (agentState a'')) Recovered
                        assertEqual "agent will stay immune for given days" (daysInState (agentState a'')) daysImmune )

tests = TestList [ TestLabel "deimmunization" testDeimmunization,
                   TestLabel "curing" testCuring  ]
