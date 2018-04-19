module SIRContTotal

import Data.Vect

import Export
import Random

-- we want this implementation to be total!
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
  -- A susceptible agent MAY become infected 
  data SusceptibleAgent = S (Double -> Inf (Either SusceptibleAgent InfectedAgent))
  -- An infected agent WILL recover after finite steps
  data InfectedAgent = I (Double -> Inf (Either InfectedAgent RecoveredAgent))
  -- A recovered agent will stay recovered FOREVER
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
-- TODO: can we ensure somehow that s + i + r = n
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

createAgents :  (s : Nat)
              -> (i : Nat)
              -> (r : Nat)
              -> RandomStream
              -> (Vect s SusceptibleAgent, Vect i InfectedAgent, Vect r RecoveredAgent)
createAgents s i r rs =
    let sus = createSus s rs
        inf = createInfs i rs
        rec = replicate r recovered
    in  (sus, inf, rec)
  where
    createSus : (s : Nat) -> RandomStream -> Vect s SusceptibleAgent
    createSus Z _ = []
    createSus (S k) rs =
      let (rs', rs'') = split rs
          sus'        = createSus k rs' 
      in  (susceptible rs'') :: sus'

    createInfs : (i : Nat) -> RandomStream -> Vect i InfectedAgent
    createInfs Z _      = []
    createInfs (S k) rs =
        let (dur, rs') = randomExp rs (1 / illnessDuration) 
            infs'      = createInfs k rs' 
        in  (infected dur) :: infs'

{-
runAgents :  Double
          -> (n : Nat) -- total number of agents
          -> Vect s SusceptibleAgent 
          -> Vect i InfectedAgent
          -> Vect (minus (s + i) n) RecoveredAgent 
          -> List (Nat, Nat, Nat)
-}

-- The idea is to implement a total agent-based SIR simulation 
-- The dynamics of the system-dynamics SIR model are in equilibrium
-- (won't change anymore) when the infected stock is 0. This can 
-- (probably) be shown formally but intuitionistic it is clear because
-- only infected agents can lead to infections of susceptible agents
-- which then make the transition to recovered after having gone
-- through the infection phase.
-- Thus an agent-based implementation of the SIR simulation has to
-- terminate if it is implemented correctly because all infected 
-- agents will recover after a finite number of steps after then 
-- the dyanimcs will be in equilibrium.
-- Thus we need to 'tell' the type-checker the following:
-- 1) no more infected agents is the termination criterion
-- 2) all infected agents will recover after a finite number of time
-- => the simulation will eventually run out of infected agents
-- But when we look at the SIR+S model we have the same termination
-- criterion, but we cannot guarantee that it will run out of infected 
-- => we need additional criteria
-- 4) infected agents are 'generated' by susceptible agents
-- 5) susceptible agents are NOT INCREASING (e.g. recovered agents 
--    do NOT turn back into susceptibles)
-- TODO: can we adopt our solution (if we find it), into a SIRS
-- implementation? this should then break totality. also how difficult
-- is it?
runAgents :   Double
           -> Vect s SusceptibleAgent 
           -> Vect i InfectedAgent
           -> Vect r RecoveredAgent 
           -> List (Nat, Nat, Nat)
runAgents dt sus inf rec = ?runAgents_rhs
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

    runAgentsAux : Vect s SusceptibleAgent
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
    
sirTotal : IO ()
sirTotal = do
  let rs = randoms 42
  let (sus, inf, rec) = createAgents 99 1 0 rs
  let dyns = runAgents 1.0 sus inf rec 
  writeMatlabFile "sirTotal.m" dyns

namespace Main
  main : IO ()
  main = sirTotal