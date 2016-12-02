module HACSimulationImpl where

import qualified HACSimulation as Sim
import qualified HACYampaBackend as YampaBack
import qualified HACClassicBackend as ClassicBack

-----------------------------------------------------------------------------------------------------------------------
-- Using Classic Backend --
-----------------------------------------------------------------------------------------------------------------------
simulationIO :: Sim.SimulationIO
simulationIO = ClassicBack.processIO

simulationStep :: Sim.SimulationStep
simulationStep = ClassicBack.processSteps

-----------------------------------------------------------------------------------------------------------------------
-- Using YAMPA --
-----------------------------------------------------------------------------------------------------------------------
{-
simulationIO :: Sim.SimulationIO
simulationIO = YampaBack.processIO

simulationStep :: Sim.SimulationStep
simulationStep = YampaBack.processSteps
-}

----------------------------------------------------------------------------------------------------------------------
