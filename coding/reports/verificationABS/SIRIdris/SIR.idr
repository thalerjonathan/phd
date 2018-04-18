module SIR 

import Data.Vect

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

-- we are using continuations for agents where each 
-- evaluation corresponds to one execution of the agent 
-- at a given time-delta, thus we have a mapping of
-- continuations to time-passing.
-- Another oportunity would be to write some EDSL with
-- GADTs and an interpreter but then we would need some
-- command in our language which tells the interpreter
-- that the agent gives its control back to the system
-- so that then the next time-step can be computed

mutual
  ||| A susceptible agent MAY become infected 
  data SusceptibleAgent = S (Double -> Inf (Either SusceptibleAgent InfectedAgent))
  ||| An infected agent WILL recover after finite steps
  data InfectedAgent = I (Double -> Inf (Either InfectedAgent RecoveredAgent))
  ||| A recovered agent will stay recovered FOREVER
  data RecoveredAgent = R (Double -> Inf RecoveredAgent)

recovered : RecoveredAgent
recovered = R (\_ => recovered)

infected : Double -> InfectedAgent
infected recoveryTime 
  = I (\dt => if recoveryTime - dt > 0
                then Left $ infected (recoveryTime - dt)
                else Right recovered)

susceptible : SusceptibleAgent
susceptible = S (\dt => Left susceptible)

-------------------------------------------------------------------------------
createAgents :  Nat 
             -> Nat 
             -> Nat
             -> RandomStream
             -> (List SusceptibleAgent, List InfectedAgent, List RecoveredAgent)
createAgents susCount infCount recCount rs
    = let sus = replicate susCount susceptible
          inf = createInfs infCount rs
          rec = replicate recCount recovered
      in  (sus, inf, rec)
  where
    createInfs : Nat -> RandomStream -> List InfectedAgent
    createInfs Z _ = []
    createInfs (S k) rs
      = let (dur, rs') = randomExp (1 / illnessDuration) rs 
            infs'      = createInfs k rs' 
        in  (infected dur) :: infs'

partial
runAgents :  Double
          -> List SusceptibleAgent 
          -> List InfectedAgent
          -> List RecoveredAgent
          -> List (Nat, Nat, Nat)
runAgents dt sas ias ras = runAgentsAux sas ias ras []
  where
    runSusceptibles :  List SusceptibleAgent 
                    -> (List SusceptibleAgent, List InfectedAgent)
                    -> (List SusceptibleAgent, List InfectedAgent)
    runSusceptibles [] acc = acc
    runSusceptibles ((S f) :: sas) (sas', ias')
      = case Force (f dt) of
            (Left l)  => runSusceptibles sas (l :: sas', ias')
            (Right r) => runSusceptibles sas (sas', r :: ias')

    runInfected :  List InfectedAgent 
                -> (List InfectedAgent, List RecoveredAgent)
                -> (List InfectedAgent, List RecoveredAgent)
    runInfected [] acc = acc
    runInfected ((I f) :: ias) (ias', ras')
      = case Force (f dt) of
            (Left l)  => runInfected ias (l :: ias', ras')
            (Right r) => runInfected ias (ias', r :: ras')

    partial
    runAgentsAux : List SusceptibleAgent 
                 -> List InfectedAgent
                 -> List RecoveredAgent
                 -> List (Nat, Nat, Nat)
                 -> List (Nat, Nat, Nat)
    runAgentsAux sas [] ras acc = reverse acc
    runAgentsAux sas ias ras acc 
      = let (sas', iasNew) = runSusceptibles sas ([], [])
            (ias', recNew) = runInfected ias ([], [])
            
            sasNext = sas'
            iasNext = iasNew ++ ias'
            recNext = recNew ++ ras

            sasCount = length sasNext
            iasCount = length iasNext
            recCount = length recNext

        in  runAgentsAux sasNext iasNext recNext ((sasCount, iasCount, recCount) :: acc)

partial
testRunAgentsList : IO ()
testRunAgentsList = do
  let rs = randoms 42
  let (sus, inf, rec) = createAgents 99 1 0 rs
  let dyns = runAgents 1.0 sus inf rec 
  print dyns

-------------------------------------------------------------------------------
{-
proveTripleSum :  (a : Nat)
               -> (b : Nat)
               -> (c : Nat)
               -> (d : Nat)
               -> a + b + c = d
proveTripleSum Z b c d = ?proofSum_rhs_1
proveTripleSum a Z c d = ?proofSum_rhs_2
proveTripleSum a b Z d = ?proofSum_rhs_3
proveTripleSum Z Z Z Z = Refl


proveTupleSum :  (a : Nat)
              -> (b : Nat)
              -> (d : Nat)
              -> a + b = d
proveTupleSum a b Z = ?alk -- only satisfied when a = b = 0
proveTupleSum Z b d = -- only satisfied when b = d
  case decEq b d of
      (Yes Refl) => Refl
      (No contra) => ?bla1_3
proveTupleSum a Z d =  -- only satisfied when a = d
  let prf = plusZeroRightNeutral d
      de  = decEq a d
  in  ?bla2  -- eqSucc a d -- ?bla2 -- plusZeroRightNeutral d
proveTupleSum (S k) (S j) (S i) = ?bla_2 -- proveTupleSum k j i -- ?bla_2
proveTupleSum Z Z Z = Refl
-}

-- TODO: can we ensure somehow that s + i + r = n
createAgentsV :  (s : Nat)
              -> (i : Nat)
              -> (r : Nat)
              -> RandomStream
              -> (Vect s SusceptibleAgent, Vect i InfectedAgent, Vect r RecoveredAgent)
createAgentsV s i r rs =
    let sus = replicate s susceptible
        inf = createInfs i rs
        rec = replicate r recovered
    in  (sus, inf, rec)

  where
    createInfs : (i : Nat) -> RandomStream -> Vect i InfectedAgent
    createInfs Z _      = []
    createInfs (S k) rs =
        let (dur, rs') = randomExp (1 / illnessDuration) rs 
            infs'      = createInfs k rs' 
        in  (infected dur) :: infs'

{-
runAgentsV :  Double
           -> (n : Nat) -- total number of agents
           -> Vect s SusceptibleAgent 
           -> Vect i InfectedAgent
           -> Vect (minus (s + i) n) RecoveredAgent 
           -> List (Nat, Nat, Nat)
-}

runAgentsV :  Double
           -> Vect s SusceptibleAgent 
           -> Vect i InfectedAgent
           -> Vect r RecoveredAgent 
           -> List (Nat, Nat, Nat)
runAgentsV dt sus inf rec = ?runAgentsV_rhs
  where
    -- TODO: encode that the length of susceptible may either
    -- stay constant in which case infected agents dont change
    -- OR it decreases by one in which case the infected agents increase by one
    -- => the sum of s and i: s + i must stay the same before and after the function call
    runSusceptibles :  Vect s SusceptibleAgent
                    -> (Vect s' SusceptibleAgent, Vect i' InfectedAgent)
    runSusceptibles [] = ([], []) -- ?runSusceptibles_rhs_1 -- ([], [])
    runSusceptibles ((S f) :: sus) = ?runSusceptibles_rhs_2
    {-
      = let (sus', inf) = runSusceptibles sus
        in  case Force (f dt) of
                (Left l)  => ?runSusceptibles_rhs_2 --(l :: sus', inf)
                (Right r) => ?runSusceptibles_rhs_3 --(sus', r :: inf)
      -}

                      {-
    runSusceptibles [] acc = acc
    runSusceptibles ((S f) :: sus) (sus', inf')
      = case Force (f dt) of
            (Left l)  => runSusceptibles sas (l :: sas', ias')
            (Right r) => runSusceptibles sas (sas', r :: ias')
    -}

    partial
    runAgentsAuxV : Vect s SusceptibleAgent
                 -> Vect i InfectedAgent
                 -> Vect r RecoveredAgent
                 -> List (Nat, Nat, Nat) 
                 -> List (Nat, Nat, Nat)
                 {-
    runAgentsAuxV sus [] rec acc = reverse acc
    runAgentsAuxV sus inf rec acc 
      = let (sus', infNew) = runSusceptibles sus ([], [])
            --(inf', recNew) = runInfected ias ([], [])
            inf' = inf
            recNew = rec
            
            susNext = sus'
            infNext = infNew ++ inf'
            recNext = recNew ++ rec

            susCount = length susNext
            infCount = length infNext
            recCount = length recNext

        in  runAgentsAuxV susNext infNext recNext ((susCount, infCount, recCount) :: acc)
    -}
    
partial
testRunAgentsVect : IO ()
testRunAgentsVect = do
  let rs = randoms 42
  let (sus, inf, rec) = createAgentsV 99 1 0 rs
  let dyns = runAgentsV 1.0 sus inf rec 
  ?testRunAgentsVect_rhs_1
  --print dyns

-------------------------------------------------------------------------------

partial
main : IO ()
main = testRunAgentsVect