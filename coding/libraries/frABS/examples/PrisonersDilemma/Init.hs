module PrisonersDilemma.Init (
    initPrisonersDilemma
  ) where

import PrisonersDilemma.Model
import PrisonersDilemma.Agent

import FRP.FrABS

import System.Random
import Control.Monad.Random

initPrisonersDilemma :: (Int, Int) -> IO ([PDAgentDef], PDEnvironment)
initPrisonersDilemma dims@(maxX, maxY) = 
  do
    let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
    let agentCount = maxX * maxY

    let aids = [0..agentCount-1]

    let aidCoordPairs = zip aids coords

    let cells = zip coords aids

    let centerX = floor $ (fromIntegral maxX) * 0.5
    let centerY = floor $ (fromIntegral maxY) * 0.5

    adefs <- mapM (runCreatePDIO (centerX, centerY)) aidCoordPairs

    envRng <- newStdGen 
    let env = createEnvironment
                          Nothing
                          dims
                          neumannSelf   -- NOTE: include self otherwise won't get the desired results
                          ClipToMax
                          cells
                          envRng
                          Nothing

    return (adefs, env)

runCreatePDIO :: (Int, Int)
                    -> (AgentId, EnvCoord) 
                    -> IO PDAgentDef
runCreatePDIO center aidCoord@(_, coord) = 
  do
    std <- getStdGen

    let a = if center == coord then Defector else Cooperator

    let (adef, std') = runRand (createPDAgent
                                    aidCoord
                                    pdAgentBehaviour
                                    a)
                                    std
    setStdGen std'
    return adef