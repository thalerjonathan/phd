import Data.Primitives.Views

%default total

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState = Susceptible | Infected | Recovered

SimulationState : Type
SimulationState = ?simulationState

contactSusceptibleWith : Int -> SIRState -> SIRState
contactSusceptibleWith _ Susceptible = Susceptible
contactSusceptibleWith _ Recovered   = Susceptible
contactSusceptibleWith rand Infected with (divides rand 100)
  contactSusceptibleWith ((100 * div) + rem) Infected | (DivBy prf) 
     = case rem <= cast (infectivity * 100) of
           False => Susceptible
           True  => Infected

data AgentCmd : (t : Type) -> SIRState -> (t -> SIRState) -> Type where
     MakeContact : (rand : Int) -> AgentCmd SIRState Susceptible (contactSusceptibleWith rand)
     Infect : AgentCmd () Susceptible (const Infected)
     Recover : AgentCmd () Infected (const Recovered)

     Pure : (res : t) -> AgentCmd t (sf res) sf
     (>>=) : AgentCmd a s1 s2_fn -> 
             ((res : a) -> AgentCmd b (s2_fn res) s3_fn) -> 
             AgentCmd b s1 s3_fn

{-
namespace SimulationIteration
  data SimIter : (t : Type) -> SimulationState -> (t -> SimulationState) -> Type where
       (>>=) : AgentCmd a s1 s2_fn -> 
               ((res : a) -> Inf (SimIter b (s2_fn res) s3_fn)) -> 
               SimIter b s1 s3_fn
       -- Terminate : SimIter () NotRunning (const NotRunning)
-}

susceptibleAgent : Stream Int -> AgentCmd () Susceptible (const Recovered)
susceptibleAgent (r :: rands) = do
  res <- MakeContact r
  case res of
       Susceptible => ?susceptibleAgent_pure_1
       Infected    => ?susceptibleAgent_pure_2
       Recovered   => ?susceptibleAgent_pure_3

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'