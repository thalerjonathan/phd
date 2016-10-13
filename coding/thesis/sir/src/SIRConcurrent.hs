module SIRConcurrent where
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import System.IO
import System.IO.Unsafe
import System.Random

data MessageType d = AgentStart | AgentStop | AgentNeighbours | AgentDomain d

data Message d = Message
  {
    msgType :: MessageType d,
    neighbours :: Maybe [TChan (Message d)],
    content :: Maybe String
  }
    
data AgentInfrastructure d = AgentInfrastructure
  {
    agentProcess :: ThreadId,
    agentMBox :: TChan (Message d)
  }

type MessageHandler s d = (Message d -> Agent s d -> Agent s d)
type ActivityHandler s d = (Agent s d -> Agent s d)

data Agent s d = Agent
  {
    agentInfra :: AgentInfrastructure d,
    agentNeighbours :: [AgentInfrastructure d],
    agentState :: s,
    agentMsgHandler :: MessageHandler s d,
    agentActivityHandler :: ActivityHandler s d
  }

_agentThreadFunc :: Agent s d -> IO ()
_agentThreadFunc aInit = do
  t <- myThreadId
  let is = agentInfra aInit
  let fullAgent = aInit { agentInfra = is { agentProcess = t } }
  putStrLn $ "Created Agent with " ++ show t ++ ", listening to incoming messages"
  listenToMBox fullAgent
  --forever $ atomically (readTChan $ agentMBox fullAgent) >>= f
  --return ()
  
createAgent :: s -> ActivityHandler s d -> MessageHandler s d -> IO (Agent s d)
createAgent initState actHandler msgHandler = do
  m <- newTChanIO
  let is = AgentInfrastructure { agentMBox = m }
  let a = Agent { agentInfra = is,  agentState = initState, agentNeighbours = [], agentMsgHandler = msgHandler, agentActivityHandler = actHandler }
  t <- forkIO $ _agentThreadFunc a
  return a { agentInfra = is { agentProcess = t } }

listenToMBox :: Agent s d -> IO ()
listenToMBox a = do
  let mbox = agentMBox (agentInfra a)
  ret <- atomically $ tryReadTChan mbox
  matchSTM a ret
  return ()

matchSTM :: Agent s d -> Maybe (Message d) -> IO ()
matchSTM a Nothing = listenToMBox a
matchSTM a (Just d) = do
  let handler = agentMsgHandler a
  let newA = handler d a
  listenToMBox newA

randomBool :: Float -> Bool
randomBool p = p >= rand
  where
    rand = unsafePerformIO (getStdRandom (randomR (0.0, 1.0)))
    
-----------------------------------------------------------------------------------------------
-- SIR-Implementation

populationCount :: Int
populationCount = 2

simStepsCount :: Int
simStepsCount = 10

infectionProb :: Float
infectionProb = 0.5

daysInfectous :: Int
daysInfectous = 3

initInfected :: Float
initInfected = 0.1

data SIRState = Susceptible | Infected | Recovered 
data SIRDomain = Contact

type SIRMessageType = MessageType SIRDomain
type SIRMessage = Message SIRMessageType
type SIRAgent = Agent SIRState SIRMessageType

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  as <- populateSIR populationCount
  return ()

sirMsgHandler :: SIRMessage -> SIRAgent -> SIRAgent
sirMsgHandler msg a = a

{- TODO: it would be useful to pass in some continuous time at this point, but need to have wall-clock instead of thread-iterations! -}
sirAgentActivity :: SIRAgent -> SIRAgent
sirAgentActivity a = a

populateSIR :: Int -> IO [SIRAgent]
populateSIR n = foldr (\i acc ->
                        do
                          a <- createSIRAgent
                          as <- acc
                          return (a:as)) (return []) [1..n]

createSIRAgent :: IO SIRAgent
createSIRAgent 
  | infected == True = createAgent Infected sirAgentActivity sirMsgHandler
  | otherwise = createAgent Susceptible sirAgentActivity sirMsgHandler
    where
      infected = randomBool initInfected


