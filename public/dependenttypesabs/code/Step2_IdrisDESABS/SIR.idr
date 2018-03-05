import Control.ST
import Data.Vect

%default total

-- NOTE: this step follows the ST approach 
--  http://docs.idris-lang.org/en/latest/st/machines.html
--  http://materials.dagstuhl.de/files/17/17051/17051.EdwinBrady.Slides.pdf

Time : Type
Time = Nat

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState = Susceptible | Infected | Recovered
data InfectionResult = Occurred | MissedOut

interface SIRAgent (m : Type -> Type) where
  Agent       : SIRState -> Nat -> Type

  -- TODO: adding an agent? this seems to be not quite correct yet
  susceptible : ST m Var [add (Agent Susceptible Z)]
  -- todo: add proof of making contact with an infected to infect
  infect      : (agent : Var) -> 
                ST m InfectionResult [agent ::: Agent Susceptible Z :-> 
                                      (\res => Agent (case res of
                                                  Occurred  => Infected
                                                  MissedOut => Susceptible) Z )]

  -- TODO: recovering has to depend on the time
  recover     : (agent : Var) -> ST m () [agent ::: Agent Infected Z :-> Agent Recovered Z]

  -- TODO: need contact-type which is a proof for contact with an infected
  makeContact : (agent : Var) -> ST m SIRState [agent ::: Agent Susceptible Z :-> Agent Susceptible Z]

SIRAgent IO where
  Agent x t = ?SIRAgent_rhs_1
  susceptible = ?SIRAgent_rhs_2
  infect agent = ?SIRAgent_rhs_3
  recover agent = ?SIRAgent_rhs_4
  makeContact agent = ?SIRAgent_rhs_5

susceptibleAgent : SIRAgent m 
                 => (agent : Var)
                 -> Time
                 -> ST m () [agent ::: Agent {m} Susceptible Z] -- why do we need {m}?
susceptibleAgent a t = do
  cont <- makeContact a
  case cont of
    Susceptible => susceptibleAgent a (S t)
    Recovered   => susceptibleAgent a (S t)
    Infected    => do
      infection <- infect a
      case infection of
        Occurred  => ?susceptibleAgent_rhs_1
        MissedOut => susceptibleAgent a (S t)

main : IO ()
main = print "Hello SIR"