import Data.Primitives.Views

%default total

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState = Susceptible | Infected | Recovered

contactSusceptibleWith : Int -> SIRState -> SIRState
contactSusceptibleWith _ Susceptible = Susceptible
contactSusceptibleWith _ Recovered   = Susceptible
contactSusceptibleWith rand Infected with (divides rand 100)
  contactSusceptibleWith ((100 * div) + rem) Infected | (DivBy prf) 
     = case rem <= cast (infectivity * 100) of
            False => Susceptible
            True  => Infected

data AgentCmd : (t : Type) -> SIRState -> (t -> SIRState) -> Type where
     MakeContacts : Nat -> AgentCmd Bool Susceptible (\res => if res then Infected else Susceptible)
     Recover : (time : Nat) -> AgentCmd () Infected (\res => case time of 
                                                             Z => Recovered
                                                             (S t) => Infected)

     {-
     Pure : (res : t) -> AgentCmd t s s
     (>>=) : AgentCmd a s1 s2 -> 
             (a -> AgentCmd b s2 s3) -> 
             AgentCmd b s1 s3
      -}

     Pure  : (res : t) -> AgentCmd t (state_fn res) state_fn
     (>>=) : AgentCmd a s1 s2_fn ->
               ((res : a) -> AgentCmd b (s2_fn res) s3_fn) ->
               AgentCmd b s1 s3_fn

namespace SimulationIteration
  data SimIter : (t : Type) -> SIRState -> (t -> SIRState) -> Type where
       (>>=) : AgentCmd a s1 s2_fn -> 
               ((res : a) -> Inf (SimIter b (s2_fn res) s3_fn)) -> 
               SimIter b s1 s3_fn
        -- Terminate : SimIter () NotRunning (const NotRunning)

data SimIO : SIRState -> Type where
  Do : AgentCmd a s1 s2_fn -> ((res : a) -> Inf (SimIO (s2_fn res))) -> SimIO s1 -- why s1 here and not s2 ???

recoveredBehaviour : AgentCmd () Recovered (const Recovered)
recoveredBehaviour = Pure ()

infectedBehaviour : Nat -> AgentCmd () Infected (const Recovered)
infectedBehaviour Z = do
  Recover Z
  recoveredBehaviour
infectedBehaviour (S k) = do
  Recover (S k)
  infectedBehaviour k

partial -- TODO: make total!
susceptibleBehaviour : AgentCmd () Susceptible (const Recovered)
susceptibleBehaviour = do
  infected <- MakeContacts contactRate
  case infected of
      False => susceptibleBehaviour
      True  => infectedBehaviour illnessDuration -- TODO: exponential distribution

randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

runAgentCmd : AgentCmd a beforeState afterState_fn -> IO a
runAgentCmd (MakeContacts n) = pure True
runAgentCmd (Recover time)   = pure ()
runAgentCmd (Pure x)         = pure x
runAgentCmd (cmd >>= cont)   = do
  ret <- runAgentCmd cmd
  runAgentCmd (cont ret)

partial
run : SimIO s -> IO ()
run (Do cmd cont) = do
  res <- runAgentCmd cmd
  run (cont res)

