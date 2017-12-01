module SIR
  (
    AgentId
  , AgentMessage
  , AgentIn
  , AgentOut

  , SIRState (..)
  , SIRMsg (..)

  , SIRAgentIn
  , SIRAgentOut
  
  , agentId
  , agentObservable

  , agentIn
  , agentOut
  , agentOutObs

  , initAgents
  
  , sendMessage
  , sendMessageM
  , onMessageM
  , onMessage
  , distributeMessages
  
  , gotInfected
  , respondToContactWithM

  , aggregateAllStates
  , aggregateStates
  , writeAggregatesToFile

  , randomBoolM
  , randomExpM
  , randomElem
  ) where

import qualified Data.Map as Map
import System.IO
import Text.Printf

import Control.Monad.Random
import Control.Monad.State

type AgentId = Int
type AgentMessage m = (AgentId, m)

data AgentIn m = AgentIn
  {
    aiId    :: AgentId
  , aiMsgs  :: [AgentMessage m]
  } deriving (Show)

data AgentOut s m = AgentOut
  {
    aoMsgs      :: [AgentMessage m]
  , aoObsState  :: Maybe s
  } deriving (Show)

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
data SIRMsg = Contact SIRState deriving (Show, Eq)
type SIRAgentIn = AgentIn SIRMsg
type SIRAgentOut = AgentOut SIRState SIRMsg

agentId :: AgentIn m -> AgentId
agentId AgentIn { aiId = aid } = aid

agentObservable :: AgentOut s m -> Maybe s
agentObservable AgentOut { aoObsState = os } = os

agentIn :: AgentId -> AgentIn m
agentIn aid = AgentIn {
    aiId    = aid
  , aiMsgs  = []
  }

agentOut :: AgentOut s m
agentOut = AgentOut {
    aoMsgs      = []
  , aoObsState  = Nothing
  }

agentOutObs :: s -> AgentOut s m
agentOutObs s = AgentOut {
    aoMsgs      = []
  , aoObsState  = Just s
  }

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

sendMessage :: AgentMessage m -> AgentOut s m -> AgentOut s m
sendMessage msg ao = ao { aoMsgs = msg : aoMsgs ao }

sendMessageM :: (Monad mo) => AgentMessage m -> StateT (AgentOut s m) mo ()
sendMessageM msg = state (\ao -> ((), sendMessage msg ao))

onMessageM :: (Monad mon) => (acc -> AgentMessage m -> mon acc) -> AgentIn m -> acc -> mon acc
onMessageM msgHdl ai acc
    | null msgs = return acc
    | otherwise = foldM msgHdl acc msgs
  where
    msgs = aiMsgs ai

onMessage :: (AgentMessage m -> acc -> acc) -> AgentIn m -> acc -> acc
onMessage msgHdl ai a 
    | null msgs = a
    | otherwise = foldr (\msg acc'-> msgHdl msg acc') a msgs
  where
    msgs = aiMsgs ai

gotInfected :: RandomGen g => Double -> SIRAgentIn -> Rand g Bool
gotInfected infectionProb ain = onMessageM gotInfectedAux ain False
  where
    gotInfectedAux :: RandomGen g => Bool -> AgentMessage SIRMsg -> Rand g Bool
    gotInfectedAux False (_, Contact Infected) = randomBoolM infectionProb
    gotInfectedAux x _ = return x

respondToContactWithM :: Monad m => SIRState -> SIRAgentIn -> StateT SIRAgentOut m ()
respondToContactWithM state ain = onMessageM respondToContactWithMAux ain ()
  where
    respondToContactWithMAux :: Monad m => () -> AgentMessage SIRMsg -> StateT SIRAgentOut m ()
    respondToContactWithMAux _ (senderId, Contact _) = sendMessageM (senderId, Contact state) 

distributeMessages :: [AgentIn m] -> [(AgentId, AgentOut s m)] -> [AgentIn m]
distributeMessages ains aouts = map (distributeMessagesAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllMessages aouts

    distributeMessagesAux :: Map.Map AgentId [AgentMessage m]
                                -> AgentIn m
                                -> AgentIn m
    distributeMessagesAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiMsgs ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (\receiverMsgs -> receiverMsgs ++ msgs) mayReceiverMsgs

        ain' = ain { aiMsgs = msgsEvt }

    collectAllMessages :: [(AgentId, AgentOut s m)] -> Map.Map AgentId [AgentMessage m]
    collectAllMessages aos = foldr collectAllMessagesAux Map.empty aos
      where
        collectAllMessagesAux :: (AgentId, AgentOut s m)
                                  -> Map.Map AgentId [AgentMessage m]
                                  -> Map.Map AgentId [AgentMessage m]
        collectAllMessagesAux (senderId, ao) accMsgs 
            | not $ null msgs = foldr collectAllMessagesAuxAux accMsgs msgs
            | otherwise = accMsgs
          where
            msgs = aoMsgs ao

            collectAllMessagesAuxAux :: AgentMessage m
                                        -> Map.Map AgentId [AgentMessage m]
                                        -> Map.Map AgentId [AgentMessage m]
            collectAllMessagesAuxAux (receiverId, m) accMsgs = accMsgs'
              where
                msg = (senderId, m)
                mayReceiverMsgs = Map.lookup receiverId accMsgs
                newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs

                -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
                accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)

aggregateAllStates :: [[SIRState]] -> [(Int, Int, Int)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Int, Int, Int)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = length $ filter (Susceptible==) as
    infectedCount = length $ filter (Infected==) as
    recoveredCount = length $ filter (Recovered==) as

writeAggregatesToFile :: String -> [(Int, Int, Int)] -> IO ()
writeAggregatesToFile fileName dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirAggregateToString) dynamics
  hPutStrLn fileHdl "];"

  hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  hPutStrLn fileHdl "infected = dynamics (:, 2);"
  hPutStrLn fileHdl "recovered = dynamics (:, 3);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "steps = length (susceptible);"
  hPutStrLn fileHdl "indices = 0 : steps - 1;"

  hPutStrLn fileHdl "figure"
  hPutStrLn fileHdl "plot (indices, susceptibleRatio.', 'color', 'blue', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, infectedRatio.', 'color', 'red', 'linewidth', 2);"
  hPutStrLn fileHdl "hold on"
  hPutStrLn fileHdl "plot (indices, recoveredRatio.', 'color', 'green', 'linewidth', 2);"

  hPutStrLn fileHdl "set(gca,'YTick',0:0.05:1.0);"
  
  hPutStrLn fileHdl "xlabel ('Time');"
  hPutStrLn fileHdl "ylabel ('Population Ratio');"
  hPutStrLn fileHdl "legend('Susceptible','Infected', 'Recovered');"

  hClose fileHdl

sirAggregateToString :: (Int, Int, Int) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%d" susceptibleCount
  ++ "," ++ printf "%d" infectedCount
  ++ "," ++ printf "%d" recoveredCount
  ++ ";"
  
randomBoolM :: RandomGen g => Double -> Rand g Bool
randomBoolM p = getRandomR (0, 1) >>= (\r -> return $ r <= p)

randomExpM :: RandomGen g => Double -> Rand g Double
randomExpM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))
  where
    avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
    avoid x = do
      r <- getRandom
      if r == x
        then avoid x
        else return r

randomElem :: RandomGen g => [a] -> Rand g a
randomElem as = getRandomR (0, len - 1) >>= (\idx -> return $ as !! idx)
  where
    len = length as