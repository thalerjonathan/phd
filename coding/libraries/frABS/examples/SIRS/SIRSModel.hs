{-# LANGUAGE Arrows #-}
module SIRS.SIRSModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import System.Random

-- debugging imports finally, to be easily removed in final version

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SIRSState = Susceptible | Infected | Recovered deriving (Eq, Show)
data SIRSMsg = Contact SIRSState

type SIRSCoord = (Int, Int)

-- TODO: move 2d discrete coordinates to env / agent instead of agentstate s: map of agentid to coords? or spatialinfo in agentin/out?

data SIRSAgentState = SIRSAgentState {
    sirsState :: SIRSState,
    sirsCoord :: SIRSCoord,
    sirsTime :: Double
} deriving (Show)

type SIRSEnvCell = AgentId
type SIRSEnvironment = Environment SIRSEnvCell ()

type SIRSAgentDef = AgentDef SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentBehaviour = AgentBehaviour SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentIn = AgentIn SIRSAgentState SIRSMsg SIRSEnvCell ()
type SIRSAgentOut = AgentOut SIRSAgentState SIRSMsg SIRSEnvCell ()
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
infectedDuration :: Double
infectedDuration = 7.0

immuneDuration :: Double
immuneDuration = 3000.0

infectionProbability :: Double
infectionProbability = 0.3
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR
------------------------------------------------------------------------------------------------------------------------
is :: SIRSAgentOut -> SIRSState -> Bool
is ao ss = (sirsState s) == ss
    where
        s = aoState ao

sirsDt :: SIRSAgentOut -> Double -> SIRSAgentOut
sirsDt ao dt
    | is ao Susceptible = ao
    | is ao Infected = handleInfectedAgent ao dt
    | otherwise = handleRecoveredAgent ao dt


infectAgent :: SIRSAgentOut -> SIRSAgentOut
infectAgent ao
    | yes = updateState ao' (\s -> s { sirsState = Infected,
                                      sirsTime = 0.0} )
    | otherwise = ao'
    where
         (ao', yes) = drawInfectionWithProb ao infectionProbability

drawInfectionWithProb :: SIRSAgentOut -> Double -> (SIRSAgentOut, Bool)
drawInfectionWithProb ao p = (ao', infect)
    where
        (infectProb, ao') = drawRandomRangeFromAgent ao (0.0, 1.0)
        infect = infectProb <= p

handleInfectedAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleInfectedAgent ao dt = if t' >= infectedDuration then
                                recoveredAgent           -- NOTE: agent has just recovered, don't send infection-contact to others
                                else
                                    randomContact gettingBetterAgent

    where
        t = (sirsTime (aoState ao))
        t' = t + dt
        recoveredAgent = updateState ao (\s -> s { sirsState = Recovered,
                                                        sirsTime = 0.0 } )
        gettingBetterAgent = updateState ao (\s -> s { sirsTime = t' } )


handleRecoveredAgent :: SIRSAgentOut -> Double -> SIRSAgentOut
handleRecoveredAgent ao dt = if t' >= immuneDuration then
                                susceptibleAgent
                                else
                                    immuneReducedAgent
    where
        t = (sirsTime (aoState ao))
        t' = t + dt
        susceptibleAgent = updateState ao (\s -> s { sirsState = Susceptible,
                                                        sirsTime = 0.0 } )
        immuneReducedAgent = updateState ao (\s -> s { sirsTime = t' } )


randomContact :: SIRSAgentOut -> SIRSAgentOut
randomContact ao = sendMessage ao' (randNeigh, (Contact Infected))
    where
        ns = FrABS.Env.Environment.neighbours (aoEnv ao) (sirsCoord (aoState ao))
        ((_, randNeigh), ao') = agentPickRandom ao ns

-- TODO: switch SF when in different states as behaviour changes
sirsAgentBehaviour :: SIRSAgentBehaviour
sirsAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        let aoAfterMsg = onMessage ain contactInfected ao
        let aoAfterTime = sirsDt aoAfterMsg 1.0
        returnA -< aoAfterTime

    where
        contactInfected :: SIRSAgentOut -> AgentMessage SIRSMsg -> SIRSAgentOut
        contactInfected a (_, Contact Infected) 
            | is a Susceptible = infectAgent a
            | otherwise = a
        contactInfected a _ = a
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- BOILER-PLATE CODE
------------------------------------------------------------------------------------------------------------------------
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
createRandomSIRSAgents max@(x,y) p =  do
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