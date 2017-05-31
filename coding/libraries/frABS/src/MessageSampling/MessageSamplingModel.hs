{-# LANGUAGE Arrows #-}
module MessageSampling.MessageSamplingModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data MessageSamplingMsg = Hello Int deriving (Eq, Show)

data MessageSamplingState = MessageSamplingState {
    msgSampCounter :: Int
} deriving (Show)

type MessageSamplingEnvCell = ()
type MessageSamplingEnvironment = Environment MessageSamplingEnvCell ()

type MessageSamplingAgentDef = AgentDef MessageSamplingState MessageSamplingMsg MessageSamplingEnvCell ()
type MessageSamplingAgentBehaviour = AgentBehaviour MessageSamplingState MessageSamplingMsg MessageSamplingEnvCell ()
type MessageSamplingAgentIn = AgentIn MessageSamplingState MessageSamplingMsg MessageSamplingEnvCell ()
type MessageSamplingAgentOut = AgentOut MessageSamplingState MessageSamplingMsg MessageSamplingEnvCell ()

type MessageSamplingAgentConversation = AgentConversationReceiver MessageSamplingState MessageSamplingMsg MessageSamplingEnvCell ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
randomRangeCounter :: (Int, Int)
randomRangeCounter = (0, 10)
------------------------------------------------------------------------------------------------------------------------

messageSamplingAgentBehaviour :: MessageSamplingAgentBehaviour
messageSamplingAgentBehaviour = proc ain ->
    do
        let ao = agentOutFromIn ain
        returnA -< ao -- TODO: implement message-sampling: sample the system below 1.0 and do message-rountrips within 1.0 time