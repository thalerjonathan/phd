import Control.ST
import Data.Vect

%default total

-- NOTE: this step follows the ST approach from http://docs.idris-lang.org/en/latest/st/machines.html

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState = Susceptible | Infected | Recovered

data InfectionResult = Occurred | MissedOut

interface SIRAgent (m : Type -> Type) where
  Agent       : SIRState -> Type

  susceptible : ST m Var [add (Agent Susceptible)]
  infect      : (agent : Var) -> 
            ST m InfectionResult [agent ::: Agent Susceptible :-> 
                                  (\res => Agent (case res of 
                                                    Occurred  => Infected
                                                    MissedOut => Susceptible))]
  -- TODO: recovering has to depend on the time
  recover     : (agent : Var) -> ST m () [agent ::: Agent Susceptible :-> Agent Recovered]

sirAgentCycle : SIRAgent m => ST m () []
sirAgentCycle = do
  agSus <- susceptible
  inf <- infect agSus
  case inf of
      Occurred => ?whatNow_1
      MissedOut => ?whatNow_2

main : IO ()
main = print "Hello SIR"