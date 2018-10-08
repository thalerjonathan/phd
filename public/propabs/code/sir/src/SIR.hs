module SIR 
  ( SIRSimCtx (..)
  , SIRState (..)

  , defaultSIRCtx
  , sirAggregate

  , initAgents
  ) where

import Control.Monad.Random

data SIRSimCtx g = SIRSimCtx
  { syCtxTimeLimit   :: !Double
  , syCtxTimeDelta   :: !Double

  , syCtxRng         :: g

  , syCtxAgentCount  :: !Int
  , syCtxInfected    :: !Int

  , syCtxContactRate :: !Double
  , syCtxInfectivity :: !Double
  , syCtxIllnessDur  :: !Double
  }

data SIRState = Susceptible | Infected | Recovered deriving (Show, Eq)

defaultSIRCtx :: RandomGen g 
              => g
              -> SIRSimCtx g 
defaultSIRCtx g = SIRSimCtx {
    syCtxTimeLimit   = 1
  , syCtxTimeDelta   = 0.1

  , syCtxRng         = g

  , syCtxAgentCount  = 1000
  , syCtxInfected    = 1

  , syCtxContactRate = 5
  , syCtxInfectivity = 0.05
  , syCtxIllnessDur  = 15
  }

sirAggregate :: [SIRState] -> (Double, Double, Double)
sirAggregate as = (sus, inf, recs)
  where
    sus  = fromIntegral $ length $ filter (==Susceptible) as
    inf  = fromIntegral $ length $ filter (==Infected) as
    recs = fromIntegral $ length $ filter (==Recovered) as

initAgents :: Int -> Int -> [SIRState]
initAgents s i = sus ++ inf
  where
    sus = replicate s Susceptible
    inf = replicate i Infected