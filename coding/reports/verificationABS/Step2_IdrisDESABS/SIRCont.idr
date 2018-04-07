module SIRCont

||| A SIR Agent is a contination which starts as
|||  Susceptible: MAY become infected
|||  Infected:    WILL recover after finite steps
|||  Recovered:   ALWAYS stays the same
data SirAgent 
  = Susceptible (t -> Either Susceptible)
  | Infected (t -> )
  | Recovered 