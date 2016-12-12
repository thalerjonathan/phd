module HACSimulationImpl where

import qualified HACSimulation as Sim
import qualified HACClassicBackend as ClassicBack

-----------------------------------------------------------------------------------------------------------------------
-- Using Classic Backend --
-----------------------------------------------------------------------------------------------------------------------
simulationIO :: Sim.SimulationIO
simulationIO = ClassicBack.processIO

simulationStep :: Sim.SimulationStep
simulationStep = ClassicBack.processSteps