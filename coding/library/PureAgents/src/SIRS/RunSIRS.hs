module SIRS.RunSIRS where

import SIRS.SIRSModel

import qualified PureAgents as PA

import Control.Monad.STM
import System.Random

--------------------------------------------------------------------------------------------------------------------------------------------------
-- EXECUTE MODEL
--------------------------------------------------------------------------------------------------------------------------------------------------
runSIRS :: IO ()
runSIRS = do
        --hSetBuffering stdin NoBuffering
        let dt = 1.0
        let agentCount = 10
        let initInfectionProb = 0.2
        let rngSeed = 42
        let steps = 10
        let g = mkStdGen rngSeed
        -- NOTE: need atomically as well, although nothing has been written yet. primarily to change into the IO - Monad
        (as, g') <- atomically $ createRandomSIRSAgents g agentCount initInfectionProb
        putStrLn "Initial:"
        printAgents as
        -- NOTE: this works for now when NOT using parallelism
        --  (as', e') <- atomically $ Agents.stepSimulation as Nothing dt steps
        --Agents.runSimulation as Nothing (outputStep dt)
        (as', hdl) <- atomically $ PA.initStepSimulation as Nothing
        runSteps hdl 6 dt
        return ()

runSteps :: SIRSSimHandle -> Int -> Double -> IO SIRSSimHandle
runSteps hdl 0 dt = return hdl
runSteps hdl n dt = do
                    (as', _, hdl') <- atomically $ PA.advanceSimulation hdl dt
                    putStrLn ("Step " ++ (show n) ++ ":")
                    printAgents as'
                    runSteps hdl' (n-1) dt

outputStep :: (Show e) => Double -> (([SIRSAgent], Maybe e) -> IO (Bool, Double))
outputStep dt (as, e) = do
                            c <- getLine
                            putStrLn c
                            putStrLn (show e)
                            printAgents as
                            return (True, dt)

printAgents :: [SIRSAgent] -> IO ()
printAgents as = do
                    mapM (putStrLn . show . PA.state) as
                    return ()

--------------------------------------------------------------------------------------------------------------------------------------------------