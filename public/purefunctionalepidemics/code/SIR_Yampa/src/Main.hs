{-# LANGUAGE Arrows #-}
module Main where

import System.IO
import Text.Printf

import Control.Monad.Random
import FRP.Yampa

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)
type SIRAgent = SF [SIRState] SIRState

contactRate :: Double
contactRate = 5.0

infectivity :: Double
infectivity = 0.05

illnessDuration :: Double
illnessDuration = 15.0

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  let t    = 150
      dt   = 0.01
      seed = 42
      g    = mkStdGen seed
      
      agentCount    = 1000
      infectedCount = 1

      as   = initAgents agentCount infectedCount
      
      --ass  = runSimulationUntil g t dt as
      --dyns = aggregateAllStates ass

  --print $ aggregateStates $ last ass
  --writeAggregatesToFile "SIR_YAMPA.m" dyns
  writeSimulationUntil g t dt as "SIR_YAMPA.m"

runSimulationUntil :: RandomGen g 
                   => g 
                   -> Time 
                   -> DTime 
                   -> [SIRState]
                   -> [[SIRState]]
runSimulationUntil g0 t dt as = embed (stepSimulation sfs as) ((), dts)
  where
    steps = floor $ t / dt
    dts   = replicate steps (dt, Nothing)

    (rngs, _) = rngSplits g0 (length as) []
    sfs       = zipWith sirAgent rngs as
    
writeSimulationUntil :: RandomGen g
                     => g
                     -> Time
                     -> DTime
                     -> [SIRState]
                     -> String
                     -> IO ()
writeSimulationUntil g0 tMax dt as fileName = do
    let (rngs, _) = rngSplits g0 (length as) []
        sfs       = zipWith sirAgent rngs as

    fileHdl <- openFile fileName WriteMode
    hPutStrLn fileHdl "dynamics = ["

    simHdl <- reactInit 
                (return ()) 
                (\_ _ outs -> do
                  let aggr = aggregateStates outs
                  hPutStrLn fileHdl (sirAggregateToString aggr)
                  return True)
                (stepSimulation sfs as)

    writeSimulationUntilAux 0 simHdl
    hPutStrLn fileHdl "];"

    writeMatlabPlot fileHdl dt 

    hClose fileHdl
  where
    writeSimulationUntilAux :: Time
                            -> ReactHandle () [SIRState]
                            -> IO ()
    writeSimulationUntilAux t simHdl
        | t >= tMax = return ()
        | otherwise = do
          _ <- react simHdl (dt, Nothing)
          writeSimulationUntilAux (t + dt) simHdl

rngSplits :: RandomGen g => g -> Int -> [g] -> ([g], g)
rngSplits g 0 acc = (acc, g)
rngSplits g n acc = rngSplits g'' (n - 1) (g' : acc)
  where
    (g', g'') = split g

stepSimulation :: [SIRAgent] -> [SIRState] -> SF () [SIRState]
stepSimulation sfs as =
    dpSwitch
      (\_ sfs' -> (map (\sf -> (as, sf)) sfs'))
      sfs
      -- if we switch immediately we end up in endless switching, so always wait for 'next'
      (switchingEvt >>> notYet) 
      stepSimulation

  where
    switchingEvt :: SF ((), [SIRState]) (Event [SIRState])
    switchingEvt = arr (\(_, newAs) -> Event newAs)

sirAgent :: RandomGen g => g -> SIRState -> SIRAgent
sirAgent g Susceptible = susceptibleAgent g
sirAgent g Infected    = infectedAgent g
sirAgent _ Recovered   = recoveredAgent

susceptibleAgent :: RandomGen g => g -> SIRAgent
susceptibleAgent g0 = 
    switch 
      -- delay the switching by 1 step, otherwise could
      -- make the transition from Susceptible to Recovered within time-step
      (susceptible g0 >>> iPre (Susceptible, NoEvent))
      (const $ infectedAgent g0)
  where
    susceptible :: RandomGen g => g -> SF [SIRState] (SIRState, Event ())
    susceptible g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()

      -- NOTE: strangely if we are not splitting all if-then-else into
      -- separate but only a single one, then it seems not to work,
      -- dunno why
      if isEvent makeContact
        then (do
          a <- drawRandomElemSF g -< as
          case a of
            Infected -> do
              i <- randomBoolSF g infectivity -< ()
              if i
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))
        else returnA -< (Susceptible, NoEvent)

infectedAgent :: RandomGen g => g -> SIRAgent
infectedAgent g = 
    switch 
      -- delay the switching by 1 step, otherwise could
      -- make the transition from Susceptible to Recovered within time-step
      (infected >>> iPre (Infected, NoEvent))
      (const recoveredAgent)
  where
    infected :: SF [SIRState] (SIRState, Event ())
    infected = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

recoveredAgent :: SIRAgent
recoveredAgent = arr (const Recovered)

randomBoolSF :: RandomGen g => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  returnA -< (r <= p)

drawRandomElemSF :: RandomGen g => g -> SF [a] a
drawRandomElemSF g = proc as -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  let len = length as
  let idx = fromIntegral len * r
  let a =  as !! floor idx
  returnA -< a

initAgents :: Int -> Int -> [SIRState]
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected

aggregateAllStates :: [[SIRState]] -> [(Double, Double, Double)]
aggregateAllStates = map aggregateStates

aggregateStates :: [SIRState] -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = fromIntegral $ length $ filter (Susceptible==) as
    infectedCount = fromIntegral $ length $ filter (Infected==) as
    recoveredCount = fromIntegral $ length $ filter (Recovered==) as

writeAggregatesToFile :: String 
                      -> DTime
                      -> [(Double, Double, Double)] 
                      -> IO ()
writeAggregatesToFile fileName dt dynamics = do
  fileHdl <- openFile fileName WriteMode
  hPutStrLn fileHdl "dynamics = ["
  mapM_ (hPutStrLn fileHdl . sirAggregateToString) dynamics
  hPutStrLn fileHdl "];"

  writeMatlabPlot fileHdl dt

  hClose fileHdl

writeMatlabPlot :: Handle 
                -> DTime
                -> IO ()
writeMatlabPlot fileHdl dt = do
  hPutStrLn fileHdl "susceptible = dynamics (:, 1);"
  hPutStrLn fileHdl "infected = dynamics (:, 2);"
  hPutStrLn fileHdl "recovered = dynamics (:, 3);"
  hPutStrLn fileHdl "totalPopulation = susceptible(1) + infected(1) + recovered(1);"

  hPutStrLn fileHdl "susceptibleRatio = susceptible ./ totalPopulation;"
  hPutStrLn fileHdl "infectedRatio = infected ./ totalPopulation;"
  hPutStrLn fileHdl "recoveredRatio = recovered ./ totalPopulation;"

  hPutStrLn fileHdl "steps = length (susceptible);"
  hPutStrLn fileHdl "indices = 0 : steps - 1;"
  hPutStrLn fileHdl $ "indices = indices ./ " ++ show (1 / dt) ++ ";"

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

sirAggregateToString :: (Double, Double, Double) -> String
sirAggregateToString (susceptibleCount, infectedCount, recoveredCount) =
  printf "%f" susceptibleCount
  ++ "," ++ printf "%f" infectedCount
  ++ "," ++ printf "%f" recoveredCount
  ++ ";"

-- NOTE: this code demonstrates that an agent could 
-- indeed make the transition from Susceptible to
-- Recovered within a single time-step because
-- switch applies the SF to switch into immediately.
-- Using delayed switch (dSwitch) does NOT help, it
-- only delays the OBSERVATION but does also apply
-- the new SF immediately, thus the solution is 
-- to use iPre which delays by 1 step by
-- 'consuming' the immediate switching 
{-
sirTest :: SIRAgent
sirTest = switch
            susceptible --  >>> iPre (Susceptible, NoEvent))
            (const infected)
  where
    susceptible :: SF [SIRState] (SIRState, Event ())
    susceptible = proc _ -> returnA -< (Infected, Event ())

    infected :: SIRAgent
    infected = switch
            (infectedAux >>> iPre (Infected, NoEvent))
            (const recovered)
      where
        infectedAux :: SF [SIRState] (SIRState, Event ())
        infectedAux = proc _ -> returnA -< (Recovered, Event ())

    recovered :: SIRAgent
    recovered = arr (const Recovered)

main :: IO ()
main = do
  let ret = embed (sirTest) ([], [(0.1, Nothing), (0.1, Nothing)])
  print ret
-}