module SIRState

%access public export

data SIRState 
  = Susceptible 
  | Infected 
  | Recovered

Eq SIRState where
  (==) Susceptible Susceptible = True
  (==) Infected Infected = True
  (==) Recovered Recovered = True
  (==) _ _ = False

Show SIRState where
  show Susceptible = "Susceptible"
  show Infected = "Infected"
  show ecovered = "Recovered"