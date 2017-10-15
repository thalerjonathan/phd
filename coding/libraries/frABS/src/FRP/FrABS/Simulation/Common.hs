module FRP.FrABS.Simulation.Common where

import FRP.Yampa

import FRP.FrABS.Environment.Definitions
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal

runEnv :: DTime -> SimulationParams e -> e -> (e, SimulationParams e)
runEnv dt params e = maybe (e, params) (runEnvAux params e) mayEnvBeh
    where
        mayEnvBeh = simEnvBehaviour params

        runEnvAux :: SimulationParams e -> e -> EnvironmentBehaviour e -> (e, SimulationParams e)
        runEnvAux params e envBeh = (e', params')
            where
                (envBeh', e') = runAndFreezeSF envBeh e dt
                params' = params { simEnvBehaviour = Just envBeh' }