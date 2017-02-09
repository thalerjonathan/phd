module WildFire.RunWFDynamic where

import WildFire.WildFireModelDynamic

import qualified Data.Map as Map
import qualified PureAgents2DDiscrete as Front

import System.Random
import System.IO
import Data.Maybe

import qualified Graphics.Gloss as GLO
import Graphics.Gloss.Interface.IO.Simulate

import qualified PureAgentsSeq as PA

winTitle = "WildFire Dynamic SEQ"
winSize = (800, 800)

runWFDynamicRendering :: IO ()
runWFDynamicRendering = do
                            let dt = 1.0
                            let xCells = 300
                            let yCells = 300
                            let rngSeed = 42
                            let cells = (xCells, yCells)
                            let g = mkStdGen rngSeed
                            let env = createEnvironment cells
                            let c = fromJust (cellByCoord env (100, 100))
                            let (a, env', g') = igniteCell g c env
                            let hdl = PA.initStepSimulation [a] env'
                            stepWithRendering hdl dt

runWFDynamicSteps :: IO ()
runWFDynamicSteps = do
                        let dt = 1.0
                        let xCells = 300
                        let yCells = 300
                        let rngSeed = 42
                        let cells = (xCells, yCells)
                        let g = mkStdGen rngSeed
                        let env = createEnvironment cells
                        let c = fromJust (cellByCoord env (100, 100))
                        let (a, env', g') = igniteCell g c env
                        let stepCount = 1000
                        let (as', _) = PA.stepSimulation [a] env dt stepCount
                        mapM (putStrLn . show . PA.state) as'
                        return ()

stepWithRendering :: WFSimHandle -> Double -> IO ()
stepWithRendering hdl dt = simulateIO (Front.display winTitle winSize)
                                GLO.white
                                10
                                hdl
                                modelToPicture
                                (stepIteration dt)

modelToPicture :: WFSimHandle -> IO GLO.Picture
modelToPicture hdl = do
                        let env = PA.extractHdlEnv hdl
                        let cs = cells env
                        let limits = cellLimits env
                        return (Front.renderFrame (map wfCellToRenderCell (Map.elems cs)) winSize limits)

stepIteration :: Double -> ViewPort -> Float -> WFSimHandle -> IO WFSimHandle
stepIteration fixedDt viewport dtRendering hdl = return (PA.advanceSimulation hdl fixedDt)

wfCellToRenderCell :: WFCell -> Front.RenderCell
wfCellToRenderCell c = Front.RenderCell { Front.renderCellCoord = (coord c),
                                                Front.renderCellColor = cs}
    where
        shade = burnable c
        cs = case (cellState c) of
                    Living -> (0.0, shade, 0.0)
                    Burning -> (shade, 0.0, 0.0)
                    Dead -> (0.5, 0.5, 0.5)