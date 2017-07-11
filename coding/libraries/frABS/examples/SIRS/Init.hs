module SIRS.Init (
    createSIRS
  ) where

import SIRS.Model
import SIRS.Agent

import FRP.Yampa

import FRP.FrABS

import System.Random

createSIRS :: (Int, Int) -> Double -> IO ([SIRSAgentDef], SIRSEnvironment)
createSIRS dims@(maxX, maxY) p =  
    do
        rng <- newStdGen

        let agentCount = maxX * maxY
        let aids = [0 .. agentCount - 1]
        let coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        let cells = zip coords aids

        adefs <- mapM (randomSIRSAgent p) cells

        let env = (createEnvironment
                        Nothing
                        dims
                        moore
                        ClipToMax
                        cells
                        rng
                        Nothing)

        return (adefs, env)

randomSIRSAgent :: Double
                    -> (EnvCoord, AgentId)
                    -> IO SIRSAgentDef
randomSIRSAgent p (pos, agentId) = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        randTime <- getStdRandom (randomR(1.0, infectedDuration))

        let isInfected = r <= p

        let (initS, t) = if isInfected then
                            (Infected, randTime)
                            else
                                (Susceptible, 0.0)

        let as = SIRSAgentState {
                    sirsState = initS,
                    sirsTime = t }

        return AgentDef { adId = agentId,
                            adState = as,
                            adBeh = sirsAgentBehaviour,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = pos,
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------