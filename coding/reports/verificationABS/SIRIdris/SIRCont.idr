module SIRCont

import Data.Vect
import Debug.Trace

import Export
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

Eq SIRState where
  (==) Susceptible Susceptible = True
  (==) Infected Infected = True
  (==) Recovered Recovered = True
  (==) _ _ = False

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
  data RecoveredAgent = R (Inf RecoveredAgent)

recovered : RecoveredAgent
recovered = R recovered

infected : Double -> InfectedAgent
infected recoveryTime 
  = I (\dt => if recoveryTime - dt > 0
                then Left $ infected (recoveryTime - dt)
                else Right recovered)

susceptible : RandomStream -> SusceptibleAgent
susceptible rs = S (\infFract => 
    let (numContacts, rs') = randomExp rs (1 / contactRate)
        (infFlag, rs'')    = makeContact (fromIntegerNat $ cast numContacts) infFract rs'
    in  if infFlag 
          then Right $ infected (fst $ randomExp rs'' (1 / illnessDuration))
          else Left $ susceptible rs'')
  where
    makeContact :  Nat 
                -> Double
                -> RandomStream
                -> (Bool, RandomStream)
    makeContact Z _ rs = (False, rs)
    makeContact (S n) infFract rs 
      = let (flag, rs') = randomBool rs (infFract * infectivity)
        in  if flag
              then (True, rs')
              else makeContact n infFract rs'
-------------------------------------------------------------------------------
partial
createAgents :  Nat 
             -> Nat 
             -> Nat
             -> RandomStream
             -> (List SusceptibleAgent, List InfectedAgent, List RecoveredAgent)
createAgents susCount infCount recCount rs
    = let sus = createSus susCount rs
          inf = createInfs infCount rs
          rec = replicate recCount recovered
      in  (sus, inf, rec)
  where
    partial
    createSus : Nat -> RandomStream -> List SusceptibleAgent
    createSus Z _ = []
    createSus (S k) rs
      = let (rs', rs'') = split rs
            sus'        = createSus k rs' 
        in  (susceptible rs'') :: sus'

    createInfs : Nat -> RandomStream -> List InfectedAgent
    createInfs Z _ = []
    createInfs (S k) rs
      = let (dur, rs') = randomExp rs (1 / illnessDuration) 
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
    runSusceptibles :  Double
                    -> List SusceptibleAgent 
                    -> (List SusceptibleAgent, List InfectedAgent)
                    -> (List SusceptibleAgent, List InfectedAgent)
    runSusceptibles _ [] acc = acc
    runSusceptibles infFract ((S f) :: sas) (sas', ias')
      = case Force (f infFract) of
            (Left l)  => runSusceptibles infFract sas (l :: sas', ias')
            (Right r) => runSusceptibles infFract sas (sas', r :: ias')

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
    runAgentsAux _ [] _ acc = reverse acc
    runAgentsAux sus inf rec acc 
      = let nSus = cast {to=Double} $ length sus
            nInf = cast {to=Double} $ length inf
            nRec = cast {to=Double} $ length rec
            nSum = (nSus + nRec + nInf)

            infFract = nInf / nSum
            (sus', infNew) = runSusceptibles infFract sus ([], [])
            (inf', recNew) = runInfected inf ([], [])
            
            susNext = sus'
            infNext = infNew ++ inf'
            recNext = recNew ++ rec

            susCount = length susNext
            infCount = length infNext
            recCount = length recNext

        in  runAgentsAux susNext infNext recNext ((susCount, infCount, recCount) :: acc)

partial
testRunAgentsList : IO ()
testRunAgentsList = do
  let rs = randoms 42
  let (sus, inf, rec) = createAgents 10000 1 0 rs
  let dyns = runAgents 1.0 sus inf rec 
  writeMatlabFile "sir.m" dyns

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
partial
createAgentsV :  (s : Nat)
              -> (i : Nat)
              -> (r : Nat)
              -> RandomStream
              -> (Vect s SusceptibleAgent, Vect i InfectedAgent, Vect r RecoveredAgent)
createAgentsV s i r rs =
    let sus = createSus s rs
        inf = createInfs i rs
        rec = replicate r recovered
    in  (sus, inf, rec)

  where
    partial
    createSus : (s : Nat) -> RandomStream -> Vect s SusceptibleAgent
    createSus Z _ = []
    createSus (S k) rs
      = let (rs', rs'') = split rs
            sus'        = createSus k rs' 
        in  (susceptible rs'') :: sus'

    createInfs : (i : Nat) -> RandomStream -> Vect i InfectedAgent
    createInfs Z _      = []
    createInfs (S k) rs =
        let (dur, rs') = randomExp rs (1 / illnessDuration) 
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
        {-
    runSusceptibles :  Vect s SusceptibleAgent
                    -> (Vect s' SusceptibleAgent, Vect i' InfectedAgent)
    runSusceptibles [] = ([], []) -- ?runSusceptibles_rhs_1 -- ([], [])
    runSusceptibles ((S f) :: sus) = ?runSusceptibles_rhs_2

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

namespace Main
  partial
  main : IO ()
  main = testRunAgentsList