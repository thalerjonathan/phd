module SIR 

import Random

%default total

data SIRState 
  = Susceptible 
  | Infected 
  | Recovered

contactRate : Double 
contactRate = 5.0

infectivity : Double
infectivity = 0.05

illnessDuration : Double
illnessDuration = 15.0

main : IO ()

-- | can we formulate the SIR so that we have a guranteed termination?
-- can we program a SIR simulation which is total
runABS : RandomGen g 
       => g 
       -> Int
       -> Int
       -> List (Double, Double, Double)
runABS g populationSize infectedCount t dt
    = aggregateAllStates $ runSimulation g  t dt as
  where
    as = initAgents populationSize infectedCount

aggregateAllStates : List (List SIRState) -> List (Double, Double, Double)
aggregateAllStates = map aggregateStates

aggregateStates : List SIRState -> (Double, Double, Double)
aggregateStates as = (susceptibleCount, infectedCount, recoveredCount)
  where
    susceptibleCount = cast $ length $ filter (Susceptible==) as
    infectedCount = cast $ length $ filter (Infected==) as
    recoveredCount = cast $ length $ filter (Recovered==) as

data SusceptibleAgent : Type -> Type where
  SusAg : (Double -> (SIRState, Either SusceptibleAgent ))

data InfectedAgent : Type -> Type where
  InfAg : (Double -> (SIRState, Either SusceptibleAgent ))

data RecoveredAgent : Type -> Type where
  RecAg : (Double -> (SIRState, Either SusceptibleAgent ))

sirAgent : RandomStream 
         -> (Double -> (SIRState, Either 

||| A susceptible agent MAY become infected 
susceptibleAgent : RandomStream 
                 -> SIRAgent
susceptibleAgent g = 
    switch
      (susceptible g) 
      (const $ infectedAgent g)
  where
    susceptible : RandomGen g => g -> SF (List SIRState) (SIRState, Event ())
    susceptible g = proc as -> do
      makeContact <- occasionally g (1 / contactRate) () -< ()

      -- NOTE: strangely if we are not splitting all if-then-else into
      -- separate but only a single one, then it seems not to work,
      -- dunno why
      if isEvent makeContact
        then (do
          a <- drawRandomElemSF g -< as
          case a of
            Just Infected -> do
              i <- randomBoolSF g infectivity -< ()
              if i
                then returnA -< (Infected, Event ())
                else returnA -< (Susceptible, NoEvent)
            _       -> returnA -< (Susceptible, NoEvent))
        else returnA -< (Susceptible, NoEvent)

||| An infected agent WILL recover after finite steps
infectedAgent : RandomGen g 
              => g 
              -> SIRAgent
infectedAgent g = 
    switch 
      infected 
      (const recoveredAgent)
  where
    infected : SF (List SIRState) (SIRState, Event ())
    infected = proc _ -> do
      recEvt <- occasionally g illnessDuration () -< ()
      let a = event Infected (const Recovered) recEvt
      returnA -< (a, recEvt)

||| A recovered agent will stay recovered FOREVER
recoveredAgent : SIRAgent
recoveredAgent = arr (const Recovered)

randomBoolSF : RandomGen g => g -> Double -> SF () Bool
randomBoolSF g p = proc _ -> do
  r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
  returnA -< (r <= p)

drawRandomElemSF : RandomGen g => g -> SF (List a) (Maybe a)
drawRandomElemSF g = proc as -> 
  if null as 
    then returnA -< Nothing
    else do
      r <- noiseR ((0, 1) :: (Double, Double)) g -< ()
      let len = length as
      let idx = fromIntegral len * r
      let a =  as !! floor idx
      returnA -< Just a

initAgents : Int -> Int -> List SIRState
initAgents n i = sus ++ inf
  where
    sus = replicate (n - i) Susceptible
    inf = replicate i Infected
