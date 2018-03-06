import Control.ST
import Data.Vect

%default total

Time : Type
Time = Nat

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState        = Susceptible | Infected | Recovered
data InfectionResult = Occurred    | MissedOut

-- TODO: express in the types
-- SUSCEPTIBLE: MAY become infected when making contact with another agent
-- INFECTED:    WILL recover after a finite number of time-steps
-- RECOVERED:   STAYS recovered all the time

-- SIR AGENT as a whole: depending on the initial state it may stay susceptible for the whole duration of the simulation
-- or it will make the transition from susceptible->infected->recovered when infection occurs

-- SIMULATION:  advanced in steps, time represented as Nat, don't go into the hassle of Double because of non-constructiveness and cannot recurr over them
--              terminates when there are no more INFECTED agents

-- TODO: agent probably has to depend on simulation time as well?
data SIRAgent : (ty : Type) -> SIRState -> (ty -> SIRState) -> Type where
  -- only susceptible agents can get infected
  Infect : SIRAgent InfectionResult Susceptible (\res => case res of
                                                            Occurred  => Infected
                                                            MissedOut => Susceptible)

  StaySusceptible : SIRAgent SIRState Susceptible (const Susceptible)
  -- todo: need a proof of infection
  BecomeInfected : SIRAgent SIRState Susceptible (const Infected)

  -- TODO: recovering has to depend on the time
  -- only infected agents can recover
  Recover : SIRAgent () Infected (const Recovered)

  -- TODO: need contact-type which is a proof for contact with an infected
  -- only susceptible agents can make contact
  MakeContact : SIRAgent SIRState Susceptible (const Susceptible) 

  Pure : (x : ty) -> SIRAgent ty (st x) st
  (>>=) : SIRAgent a st1 st2 -> ((x : a) -> SIRAgent b (st2 x) st3) -> SIRAgent b st1 st3


-- an infected agent ALWAYS recovers after some finite time
infectedAgent : Nat -> SIRAgent () Infected (const Recovered)
infectedAgent Z     = Recover
infectedAgent (S t) = infectedAgent t

-- a susceptibleAgent MAY become infected which depends on the contact
susceptibleAgent : SIRAgent () Susceptible (const Recovered)
susceptibleAgent = do
  c <- MakeContact
  case c of 
    Susceptible => susceptibleAgent
    Recovered   => susceptibleAgent 
    Infected    => do
      infection <- Infect
      case infection of
        MissedOut => susceptibleAgent
        Occurred  => infectedAgent illnessDuration

susceptibleAgent' : SIRAgent SIRState Susceptible id
susceptibleAgent' = do
  c <- MakeContact
  case c of
    Susceptible => susceptibleAgent'
    Recovered   => susceptibleAgent' 
    Infected    => do
      infection <- Infect
      case infection of
        MissedOut => StaySusceptible -- susceptibleAgent'
        Occurred  => BecomeInfected -- Infected --?susceptibleAgent' --Infected --infectedAgent illnessDuration

runAgent : SIRAgent ty pre (const post) -> IO ()
runAgent BecomeInfected = ?bla
runAgent StaySusceptible = ?runAgent_rhs_0
runAgent Recover = ?runAgent_rhs_1
runAgent MakeContact = ?runAgent_rhs_2
runAgent (Pure x) = ?runAgent_rhs_3
runAgent (act >>= cont) = ?runAgent_rhs_4

main : IO ()
main = print "Hello SIR"