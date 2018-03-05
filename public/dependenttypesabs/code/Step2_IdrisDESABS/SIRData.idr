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

data SIRAgent : (ty : Type) -> SIRState -> (ty -> SIRState) -> Type where
  -- only susceptible agents can get infected
  Infect : SIRAgent InfectionResult Susceptible (\res => case res of
                                                                Occurred  => Infected
                                                                MissedOut => Susceptible)

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

-- a susceptibleAgent MAY become infected which in turn WILL recovers after some finite time
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

runAgent : SIRAgent ty pre (const post) -> IO ()
runAgent Recover = ?runAgent_rhs_1
runAgent MakeContact = ?runAgent_rhs_2
runAgent (Pure x) = ?runAgent_rhs_3
runAgent (act >>= cont) = ?runAgent_rhs_4

main : IO ()
main = print "Hello SIR"