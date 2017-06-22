module SIRS.SIRSInit where

import SIRS.SIRSModel
import SIRS.SIRSAgent

import FRP.Yampa

import FrABS.Agent.Agent
import FrABS.Env.Environment

import System.Random

createSIRSEnv :: (Int, Int) -> [SIRSAgentDef] -> IO SIRSEnvironment
createSIRSEnv limits as =  
    do
        rng <- newStdGen
        return (createEnvironment
                        Nothing
                        limits
                        moore
                        ClipToMax
                        cs
                        rng
                        Nothing)
    where
        cs = map (\a -> ((sirsCoord (adState a)), (adId a))) as

createRandomSIRSAgents :: (Int, Int) -> Double -> IO [SIRSAgentDef]
createRandomSIRSAgents max@(x,y) p = do
                                        let ssIO = [ randomAgentState p (xCoord, yCoord) | xCoord <- [0..x-1], yCoord <- [0..y-1] ]
                                        ss <- mapM id ssIO
                                        as <- mapM (\s -> createAgent s max) ss
                                        return as
    where
        createAgent :: SIRSAgentState -> (Int, Int) -> IO SIRSAgentDef
        createAgent s max = do 
                                rng <- newStdGen

                                return AgentDef { adId = agentId,
                                                    adState = s,
                                                    adBeh = sirsAgentBehaviour,
                                                    adInitMessages = NoEvent,
                                                    adConversation = Nothing,
                                                    adEnvPos = c,
                                                    adRng = rng }
            where
                c = sirsCoord s
                agentId = coordToAid max c

randomAgentState :: Double -> SIRSCoord -> IO SIRSAgentState
randomAgentState p coord = do
                                r <- getStdRandom (randomR(0.0, 1.0))
                                let isInfected = r <= p

                                let s = if isInfected then
                                            Infected
                                            else
                                                Susceptible

                                randTime <- getStdRandom (randomR(1.0, infectedDuration))

                                let t = if isInfected then
                                            randTime
                                            else
                                                0.0

                                return SIRSAgentState{
                                        sirsState = s,
                                        sirsCoord = coord,
                                        sirsTime = t }


coordToAid :: (Int, Int) -> SIRSCoord -> AgentId
coordToAid (xMax, _) (x, y) = (y * xMax) + x
------------------------------------------------------------------------------------------------------------------------