{-# LANGUAGE Arrows #-}
module SugarScape.SugarScapeModel where

-- Project-internal import first
import FrABS.Agent.Agent
import FrABS.Env.Environment

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then

-- debugging imports finally, to be easily removed in final version
import System.Random

-- TODO: export dynamics in a text file with matlab format of the data: wealth distribution, number of agents, mean vision/metabolism, mean age,

-- TODO random iteration in sequential
-- TODO implement rules as SF which can be turned on or off
-- TODO formalize rules in my EDSL
-- TODO problem of sugarscape trading in our functional approach: cannot reply immediately thus potentially violating budget constraints. need to solve this e.g. by having a temporary reserved amount "open for transaction"

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SugarScapeAgentGender = Male | Female deriving (Show, Eq)

data SugarScapeMsg =
    MatingRequest SugarScapeAgentGender
    | MatingReplyNo
    | MatingReplyYes (Double, Double, Int) -- Sugar-Contribution, Metabolism, Vision
    | MatingChild AgentId

    | InheritSugar Double

    deriving (Show)

data SugarScapeAgentState = SugarScapeAgentState {
    sugAgMetabolism :: Double,              -- this amount of sugar will be consumed by the agent in each time-step
    sugAgVision :: Int,                     -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete

    sugAgSugarLevel :: Double,              -- the current sugar holdings of the agent, if 0 then the agent starves to death
    sugAgSugarInit :: Double,               -- agent is fertile only when its sugarlevel is GE than its initial endowment

    sugAgMaxAge :: Double,                  -- at this age the agent will die and create a single new random offspring

    sugAgGender :: SugarScapeAgentGender,   -- an agent is either male or female
    sugAgFertAgeRange :: (Double, Double),   -- an agent younger/older than this cannot bear children

    sugAgChildren :: [AgentId],             -- the ids of all the children this agent has born

    sugAgAge :: Double,                     -- the current age of the agent, could be calculated using time in the SF but we need it in the conversations as well, which are not running in the SF

    sugAgRng :: StdGen
} deriving (Show)

data SugarScapeEnvCell = SugarScapeEnvCell {
    sugEnvSugarCapacity :: Double,
    sugEnvSugarLevel :: Double,
    sugEnvPolutionLevel :: Double,
    sugEnvOccupied :: Maybe AgentId
} deriving (Show)

type SugarScapeEnvironment = Environment SugarScapeEnvCell
type SugarScapeEnvironmentBehaviour = EnvironmentBehaviour SugarScapeEnvCell

type SugarScapeAgentDef = AgentDef SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentBehaviour = AgentBehaviour SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentIn = AgentIn SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
type SugarScapeAgentOut = AgentOut SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell

type SugarScapeAgentConversation = AgentConversationReceiver SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II
------------------------------------------------------------------------------------------------------------------------
sugarGrowbackUnits :: Double
sugarGrowbackUnits = 1.0

summerSeasonGrowbackRate :: Double
summerSeasonGrowbackRate = 1.0

winterSeasonGrowbackRate :: Double
winterSeasonGrowbackRate = 8.0

seasonDuration :: Double
seasonDuration = 50.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (0.0, 4.0)

-- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = (5.0, 25.0)

metabolismRange :: (Double, Double)
metabolismRange = (1.0, 4.0)

visionRange :: (Int, Int)
visionRange = (1, 6)

ageRange :: (Double, Double)
ageRange = (60, 100)

polutionMetabolismFactor :: Double
polutionMetabolismFactor = 1.0

polutionHarvestFactor :: Double
polutionHarvestFactor = 1.0

diffusePolutionTime :: Int
diffusePolutionTime = 25

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III
------------------------------------------------------------------------------------------------------------------------
childBearingMinAgeRange :: (Double, Double)
childBearingMinAgeRange = (12, 15)

childBearingFemaleMaxAgeRange :: (Double, Double)
childBearingFemaleMaxAgeRange = (40, 50)

childBearingMaleMaxAgeRange :: (Double, Double)
childBearingMaleMaxAgeRange = (50, 60)

sexualReproductionInitialEndowmentRange :: (Double, Double)
sexualReproductionInitialEndowmentRange = (50, 100)
------------------------------------------------------------------------------------------------------------------------


randomAgent :: (AgentId, EnvCoord)
                -> SugarScapeAgentBehaviour
                -> SugarScapeAgentConversation
                -> StdGen
                -> (SugarScapeAgentDef, StdGen)
randomAgent (agentId, coord) beh conv g0 = (adef, g8)
    where
        (randMeta, g1) = randomR metabolismRange g0
        (randVision, g2) = randomR visionRange g1
        -- (randSugarEndowment, g3) = randomR sugarEndowmentRange g2
        (randSugarEndowment, g3) = randomR sexualReproductionInitialEndowmentRange g2
        (randMaxAge, g4) = randomR ageRange g3
        (randMale, g5) = random g4 :: (Bool, StdGen)
        (randMinFert, g6) = randomR childBearingMinAgeRange g5

        randGender = if randMale then Male else Female
        fertilityMaxRange = if randMale then childBearingMaleMaxAgeRange else childBearingFemaleMaxAgeRange

        (randMaxFert, g7) = randomR fertilityMaxRange g6

        (rng, g8) = split g7

        s = SugarScapeAgentState {
            sugAgMetabolism = randMeta,
            sugAgVision = randVision,

            sugAgSugarLevel = randSugarEndowment,
            sugAgSugarInit = randSugarEndowment,

            sugAgMaxAge = randMaxAge,

            sugAgGender = randGender,
            sugAgFertAgeRange = (randMinFert, randMaxFert),

            sugAgChildren = [],

            sugAgAge = 0.0,

            sugAgRng = rng
        }

        adef = AgentDef {
           adId = agentId,
           adState = s,
           adEnvPos = coord,
           adConversation = Just conv,
           adInitMessages = NoEvent,
           adBeh = beh }