module Zombies.Run (
    runZombiesWithRendering,
    runZombiesStepsAndWriteToFile
  ) where

import FRP.Yampa

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
dt = 1.0  -- NOTE: this model has no time-semantics (it does not matter if it is 1.0 or 0.1)
frequency = 0
t = 200

-- TODO: repair

runZombiesWithRendering :: IO ()
runZombiesWithRendering =
    do
        params <- initSimulation updateStrat Nothing Nothing shuffleAgents (Just rngSeed)
        (initAdefs, initEnv) <- initZombies

        simulateAndRender initAdefs
                            initEnv
                            params
                            dt
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

        let asenv = simulateTime initAdefs initEnv params dt t

        writeDynamics asenv

writeDynamics :: [(Time, [ZombiesAgentObservable], ZombiesEnvironment)] -> IO ()
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
        writeDynamicsAux :: (Time, [ZombiesAgentObservable], ZombiesEnvironment) -> String
        writeDynamicsAux (t, aos, _) = show t ++ ", " ++ show humanCount ++ "," ++ show zombieCount ++ ";"
            where
                humanCount = length $ filter (isHuman . snd) aos
                zombieCount = length $ filter (isZombie . snd) aos

printDynamics :: (Time, [ZombiesAgentObservable], ZombiesEnvironment)
                    ->(Time, [ZombiesAgentObservable], ZombiesEnvironment)
                    -> IO ()
printDynamics (_, _, _) (_, aobsCurr, _) = 
    do
        let humanCount = length $ filter (isHuman . snd) aobsCurr
        let zombieCount = length $ filter (isZombie . snd) aobsCurr
        putStrLn (show humanCount ++ "," ++ show zombieCount ++ ";")

