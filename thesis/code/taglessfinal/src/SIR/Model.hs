module SIR.Model where

import SIR.API

data SIRState
  = Susceptible
  | Infected
  | Recovered
  deriving (Show, Eq)

data SIREvent 
  = MakeContact
  | Contact !AgentId !SIRState
  | Recover 
  deriving (Show, Eq)

createSIRStates :: Int -> Int -> Int -> [SIRState]
createSIRStates s i r = ss ++ is ++ rs
  where
    ss = replicate s Susceptible
    is = replicate i Infected
    rs = replicate r Recovered 
