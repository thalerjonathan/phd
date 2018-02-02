import Data.Primitives.Views

%default total

--  add agent-step command which transports some time

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

namespace SimulationStepping
  data SimStep : (t : Type) -> SIRState -> (t -> SIRState) -> Type where
       (>>=) : AgentCmd a s1 s2_fn -> 
               ((res : a) -> Inf (SimStep b (s2_fn res) s3_fn)) -> 
               SimStep b s1 s3_fn
       Terminate : SIRState -> SimStep SIRState s s_fn

-- a recovered agent always stays recovered
recoveredBehaviour : AgentCmd () Recovered (const Recovered)
recoveredBehaviour = Pure ()

-- an infected agent ALWAYS recovers after a finite time 
infectedBehaviour : Nat -> AgentCmd () Infected (const Recovered)
infectedBehaviour Z = do
  Recover Z
  recoveredBehaviour
infectedBehaviour (S k) = do
  Recover (S k)
  infectedBehaviour k

-- TODO: a susceptible agent can also stay susceptible all the time and never become infected!
partial -- TODO: make total!
susceptibleBehaviour : AgentCmd () Susceptible (const Recovered)
susceptibleBehaviour = do
  infected <- MakeContacts contactRate
  case infected of
      False => susceptibleBehaviour
      True  => infectedBehaviour illnessDuration -- TODO: exponential distribution

-- this was taken from the book "Type-Driven Development with Idris" by Edwin Brady
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

-- TODO: only the infected agents have influence on the system: 
-- if there are no more infected agents, the simulation should terminate

-- TODO: get rid of IO
runAgentCmd : AgentCmd a beforeState afterState_fn -> IO a
runAgentCmd (MakeContacts n) = pure True
runAgentCmd (Recover time)   = pure ()
runAgentCmd (Pure x)         = pure x
runAgentCmd (cmd >>= cont)   = do
  ret <- runAgentCmd cmd
  runAgentCmd (cont ret)

SimulationState : Type
SimulationState = ?simulationState

simulationLoop : SimulationState ->
                 SimStep t s s_fn ->
                 t
simulationLoop = ?simulationLoop_rhs

-- no need to run in IO as the whole simulation does
-- never need input from the world
run : SimStep t s1 s2_fn -> t
run (Terminate s) = s
run (agentCmd >>= cont) = ?run_rhs_1

-- TODO: implement export of dynamics to file
writeToFile : IO ()

partial
main : IO ()
main = ?main -- run (simulationLoop {pounds = 0} {chocs = 1})