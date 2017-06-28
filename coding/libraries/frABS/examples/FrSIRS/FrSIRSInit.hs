module FrSIRS.FrSIRSInit where

import FrSIRS.FrSIRSModel
import FrSIRS.FrSIRSAgent

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createFrSIRS :: (Int, Int) -> Double -> IO ([FrSIRSAgentDef], FrSIRSEnvironment)
createFrSIRS dims@(maxX, maxY) p =  
    do
        rng <- newStdGen

        let agentCount = maxX * maxY
        let aids = [0 .. agentCount - 1]
        let coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]
        let cells = zip coords aids

        adefs <- mapM (randomFrSIRSAgent p) cells

        let env = (createEnvironment
                        Nothing
                        dims
                        moore
                        ClipToMax
                        cells
                        rng
                        Nothing)

        return (adefs, env)

randomFrSIRSAgent :: Double
                    -> (EnvCoord, AgentId)
                    -> IO FrSIRSAgentDef
randomFrSIRSAgent p (pos, agentId) = 
    do
        rng <- newStdGen
        r <- getStdRandom (randomR(0.0, 1.0))
        
        let isInfected = r <= p
        let initS = if isInfected then Infected else Susceptible

        return AgentDef { adId = agentId,
                            adState = initS,
                            adBeh = (sirsAgentBehaviour rng initS),    -- for testing Yampa-implementation of Agent
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = pos,
                            adRng = rng }
------------------------------------------------------------------------------------------------------------------------