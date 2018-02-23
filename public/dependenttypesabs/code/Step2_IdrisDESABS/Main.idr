%default total

--  add agent-step command which transports some time

infectivity : Double
infectivity = 0.05

contactRate : Nat
contactRate = 5

illnessDuration : Nat
illnessDuration = 15

data SIRState = Susceptible | Infected | Recovered

interface SIRAgents (m : Type -> Type) where
  Agent : SIRState -> Type

main : IO ()
main = print "Hello SIR"