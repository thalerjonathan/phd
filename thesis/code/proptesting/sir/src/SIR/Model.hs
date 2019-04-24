module SIR.Model where

data SIRState
  = Susceptible
  | Infected
  | Recovered
  deriving (Show, Eq)