module SIR.Model where

data SIRState
  = Susceptible
  | Infected
  | Recovered
  deriving (Show, Eq)

aggregateSIRStates :: [SIRState] -> (Int, Int, Int)
aggregateSIRStates as = (sus, inf, recs)
  where
    sus  = length $ filter (==Susceptible) as
    inf  = length $ filter (==Infected) as
    recs = length $ filter (==Recovered) as

int3ToDbl3 :: (Int, Int, Int) -> (Double, Double, Double) 
int3ToDbl3 (x,y,z) = (fromIntegral x, fromIntegral y, fromIntegral z)