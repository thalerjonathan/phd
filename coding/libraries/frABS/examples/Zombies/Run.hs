module Zombies.Run (
    runZombiesWithRendering,
    runZombiesStepsAndWriteToFile
  ) where

import Zombies.Environment
import Zombies.Init
import Zombies.Renderer 
import Zombies.Model

import FRP.FrABS

import System.IO

winSize = (800, 800)
winTitle = "Zombies"
updateStrat = Sequential
shuffleAgents = False
rngSeed = 42
samplingTimeDelta = 1.0  -- NOTE: this model has no time-semantics (it does not matter if it is 1.0 or 0.1)
frequency = 0
steps = 200

runZombiesWithRendering :: IO ()
runZombiesWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initZombies

        simulateAndRender initAdefs
                            initEnv
                            params
                            samplingTimeDelta
                            frequency
                            winTitle
                            winSize
                            renderZombiesFrame
                            (Just printDynamics)

runZombiesStepsAndWriteToFile :: IO ()
runZombiesStepsAndWriteToFile =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initZombies

        let asenv = processSteps initAdefs initEnv params samplingTimeDelta steps

        writeDynamics asenv

writeDynamics :: [([ZombiesAgentObservable], ZombiesEnvironment)] -> IO ()
writeDynamics dynamics =
    do
        let fileName = "zombies_dynamics.m"

        fileHdl <- openFile fileName WriteMode
        
        hPutStrLn fileHdl "dynamics = ["
        mapM_ (hPutStrLn fileHdl . writeDynamicsAux) dynamics
        hPutStrLn fileHdl "];"

        hPutStrLn fileHdl "humanCount = dynamics (:, 1);"
        hPutStrLn fileHdl "zombieCount = dynamics (:, 2);"
        hPutStrLn fileHdl "figure"
        hPutStrLn fileHdl "plot (humanCount.', 'color', 'blue');"
        hPutStrLn fileHdl "hold on"
        hPutStrLn fileHdl "plot (zombieCount.', 'color', 'red');"
        hPutStrLn fileHdl "xlabel ('Steps');"
        hPutStrLn fileHdl "ylabel ('Agents');"
        hPutStrLn fileHdl "legend('Humans','Zombies');"
        hPutStrLn fileHdl ("title ('Zombie Dynamics');")

        hClose fileHdl

    where
        writeDynamicsAux :: ([ZombiesAgentObservable], ZombiesEnvironment) -> String
        writeDynamicsAux (aos, _) = show humanCount ++ "," ++ show zombieCount ++ ";"
            where
                humanCount = length $ filter (isHuman . snd) aos
                zombieCount = length $ filter (isZombie . snd) aos

printDynamics :: ([ZombiesAgentObservable], ZombiesEnvironment)
                    ->([ZombiesAgentObservable], ZombiesEnvironment)
                    -> IO ()
printDynamics (_, _) (aobsCurr, _) = 
    do
        let humanCount = length $ filter (isHuman . snd) aobsCurr
        let zombieCount = length $ filter (isZombie . snd) aobsCurr
        putStrLn (show humanCount ++ "," ++ show zombieCount ++ ";")

