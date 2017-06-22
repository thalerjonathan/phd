module FrABS.Simulation.SimulationUtils (
	initRng,
	initSimParams
  ) where

import FrABS.Simulation.Simulation

import Control.Monad.Random

initRng :: Int -> IO StdGen
initRng seed =
    do
        let g = mkStdGen seed
        setStdGen g
        return g

initSimParams :: UpdateStrategy
				-> Maybe (EnvironmentCollapsing ec l)
				-> Bool
				-> IO (SimulationParams ec l)
initSimParams updtStrat collFunc shuffAs = 
    do
        rng <- getSplit

        return SimulationParams {
            simStrategy = updtStrat,
            simEnvCollapse = collFunc,
            simShuffleAgents = shuffAs,
            simRng = rng
        }