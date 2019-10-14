{-# LANGUAGE Arrows     #-}
module Main where

import           Data.Maybe
import           System.IO

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats
import           Control.Monad.Random
import           Control.Monad.Reader
import qualified Data.Map as Map
import           FRP.BearRiver
import           Text.Printf

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

data SIRMsg = SusceptibleMakesContact | ContactWithInfected

type AgentId      = Int
type MsgBox       = TChan (AgentId, SIRMsg)
type AgentMBoxes  = Map.Map AgentId MsgBox
type SIRMonad g   = RandT g STM
type SIRAgent g   = SF (SIRMonad g) () SIRState

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

rngSeed :: Int
rngSeed = 42

-- TO RUN: clear & stack exec -- Step81-STM-Messages +RTS -N -s

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let dt            = 0.1
      t             = 100
      g             = mkStdGen rngSeed
      agentCount    = 10000
      infectedCount = 1

  ass <- runSimulation g t dt agentCount infectedCount

  -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently
  dumpSTMStats

  let dyns      = aggregateAllStates ass
      fileName  =  "Step81_STM_Messages" ++ show agentCount ++ "agents.m"
  
  writeAggregatesToFile fileName dyns
  -- render dt es

runSimulation :: RandomGen g
              => g 
              -> Time 
              -> DTime 
              -> Int
              -> Int
              -> IO [[SIRState]]
runSimulation g0 t dt agentCount infectedCount = do
    let (rngs, _) = rngSplits g0 agentCount []
        steps     = floor $ t / dt
        ais       = [0..agentCount-1]
        susAs     = replicate (agentCount - infectedCount) Susceptible
        infAs     = replicate infectedCount Infected
        as        = susAs ++ infAs
        mboxes0    = Map.empty 

    mboxes <- foldM (\acc aid -> do
      mb <- newTChanIO
      return $ Map.insert aid mb acc) mboxes0 ais

    vars <- zipWithM (\g' a -> do
      dtVar <- newEmptyMVar
      retVar <- sirAgentThread steps mboxes dtVar g' a
      return (dtVar, retVar)) rngs (zip ais as)

    let (dtVars, retVars) = unzip vars

    forM [0..steps-1] (simulationStep dtVars retVars)

  where
    rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
    rngSplits g 0 acc = (acc, g)
    rngSplits g n acc = rngSplits g'' (n-1) (g' : acc)
      where
        (g', g'') = split g

    simulationStep :: [MVar DTime]
                   -> [MVar SIRState]
                   -> Int
                   -> IO [SIRState]
    simulationStep dtVars retVars _i = do
      -- putStrLn $ "Step " ++ show i
      -- tell all threads to continue with the corresponding DTime
      mapM_ (`putMVar` dt) dtVars
      -- wait for results
      mapM takeMVar retVars

sirAgentThread :: RandomGen g 
               => Int
               -> AgentMBoxes
               -> MVar DTime
               -> g
               -> (AgentId, SIRState)
               -> IO (MVar SIRState)
sirAgentThread steps mboxes dtVar rng0 a@(aid, _) = do
    let mb = fromJust $ Map.lookup aid mboxes
        sf = uncurry (sirAgent mboxes mb) a

    -- create the var where the result will be posted to
    retVar <- newEmptyMVar
    _ <- forkIO $ sirAgentThreadAux steps sf rng0 retVar
    return retVar
  where
    sirAgentThreadAux :: RandomGen g 
                      => Int
                      -> SIRAgent g
                      -> g
                      -> MVar SIRState
                      -> IO ()
    sirAgentThreadAux 0 _ _ _ = return ()
    sirAgentThreadAux n sf rng retVar = do
      -- wait for next dt to compute next step
      dt <- takeMVar dtVar

      -- compute next step
      let sfReader = unMSF sf ()
          sfRand   = runReaderT sfReader dt
          sfSTM    = runRandT sfRand rng
      ((s, sf'), rng') <- trackSTM sfSTM -- atomically sfSTM -- trackSTM sfSTM
      -- NOTE: running STM with stats results in considerable lower performance the more STM actions are run concurrently

      -- post result to main thread
      putMVar retVar s
      
      sirAgentThreadAux (n - 1) sf' rng' retVar

sirAgent :: RandomGen g 
         => AgentMBoxes
         -> MsgBox
         -> AgentId
         -> SIRState 
         -> SIRAgent g
sirAgent mboxes mb aid Susceptible = susceptibleAgent mboxes mb aid
sirAgent mboxes mb aid Infected    = infectedAgent mboxes mb aid
sirAgent _      mb _   Recovered   = recoveredAgent mb

emptyMbox :: MsgBox -> STM ()
emptyMbox mbox = do
  ret <- tryReadTChan mbox
  unless (isNothing ret) (emptyMbox mbox)
     
susceptibleAgent :: RandomGen g 
                 => AgentMBoxes
                 -> MsgBox
                 -> AgentId
                 -> SIRAgent g
susceptibleAgent mboxes mb aid = 
    switch
      susceptible
      (const $ infectedAgent mboxes mb aid)
  where
    susceptible :: RandomGen g 
                => SF (SIRMonad g) () (SIRState, Event ())
    susceptible = proc _ -> do
      makeContact <- occasionally (1 / contactRate) () -< ()
      
      if isEvent makeContact 
        then arrM_ (lift makeRandomContact) -< ()
        else returnA -< ()

      inf <- arrM_ (lift checkIncomingContacts) -< ()
      if inf
        then returnA -< (Infected, Event ())
        else returnA -< (Susceptible, NoEvent)

    makeRandomContact :: RandomGen g 
                      => RandT g STM ()
    makeRandomContact = do
      let n = Map.size mboxes
      randId <- getRandomR (0, n - 1)

      let to = fromJust $ Map.lookup randId mboxes
      lift $ writeTChan to (aid, SusceptibleMakesContact)

    checkIncomingContacts :: RandomGen g 
                          => RandT g STM Bool
    checkIncomingContacts = do
      ret <- lift $ tryReadTChan mb
      case ret of
        Nothing       -> return False
        Just (_, msg) ->
          case msg of 
            ContactWithInfected -> do
              inf <- randomBool infectivity
              if inf
                then do
                  lift $ emptyMbox mb -- need to empty the box at this point because might not have consumed all
                  return True
                else checkIncomingContacts
            _ -> checkIncomingContacts

infectedAgent :: RandomGen g 
              => AgentMBoxes
              -> MsgBox
              -> AgentId
              -> SIRAgent g
infectedAgent mboxes mb aid = 
    switch
    infected 
      (const $ recoveredAgent mb)
  where
    infected :: RandomGen g => SF (SIRMonad g) () (SIRState, Event ())
    infected = proc _ -> do
      recovered <- occasionally illnessDuration () -< ()
      if isEvent recovered
        then do
          arrM_ (lift $ lift $ emptyMbox mb) -< ()
          returnA -< (Recovered, Event ())
        else do 
          arrM_ (lift $ lift replyWithInfected) -< ()
          returnA -<  (Infected, NoEvent)

    replyWithInfected :: STM ()
    replyWithInfected = do
      ret <- tryReadTChan mb
      case ret of
        Nothing  -> return ()
        Just msg -> do
          replyTo msg
          replyWithInfected

    replyTo :: (AgentId, SIRMsg) -> STM ()
    replyTo (senderId, SusceptibleMakesContact) = do
      let to = fromJust $ Map.lookup senderId mboxes
      writeTChan to (aid, ContactWithInfected)
    replyTo _ = return ()

recoveredAgent :: RandomGen g 
               => MsgBox
               -> SIRAgent g
recoveredAgent mb = proc _ -> do
    arrM_ (lift $ lift $ emptyMbox mb) -< ()
    returnA -< Recovered

randomBool :: MonadRandom m => Double -> m Bool
randomBool p = getRandomR (0, 1) >>= (\r -> return $ r <= p)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
aggregateAllStates :: [[SIRState]] -> [(Double, Double, Double)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter (Susceptible==) as
    infectedCount = fromIntegral $ length $ filter (Infected==) as
    recoveredCount = fromIntegral $ length $ filter (Recovered==) as

writeAggregatesToFile :: String -> [(Double, Double, Double)] -> IO ()
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

sirAggregateToString :: (Double, Double, Double) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"
-------------------------------------------------------------------------------