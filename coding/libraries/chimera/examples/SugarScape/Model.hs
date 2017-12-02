module SugarScape.Model (
    SugarScapeAgentGender (..),
    SugarScapeTribe (..),
    SugarScapeCulturalTag,
    SugarScapeCredit,
    SugarScapeCreditInfo,
    SugarScapeImmuneSystem,
    SugarScapeDisease,
    SugarScapeMsg (..),
    MatingReplyTuple,

    SugarScapeAgentState (..),

    SugarScapeEnvCellOccupier (..),
    SugarScapeEnvCell (..),

    SugarScapeEnvironment,
    SugarScapeEnvironmentBehaviour,
    SugarScapeEnvironmentMonadicBehaviour,

    SugarScapeAgentDef,
    SugarScapeAgentBehaviour,
    SugarScapeAgentIn,
    SugarScapeAgentOut,
    SugarScapeAgentObservable,
    
    SugarScapeAgentConversation,
    SugarScapeAgentConversationSender,
    SugarScapeSimParams,

    AgentColoring (..),
    _enablePolution_,
    _enableSpice_,
    _enableBirthAgentOnAgeDeath_,
    _enableInheritance_,
    _enableSex_,
    _enableCombat_,
    _enableDiseases_,
    _enableSeasons_,
    _enableTrading_,
    _enableCredit_,
    _agentColoring_,

    sugarGrowbackUnits,
    summerSeasonSugarGrowbackRatio, 
    winterSeasonSugarGrowbackRatio,
    seasonDuration,
    sugarCapacityRange,
    sugarEndowmentRange,
    sugarMetabolismRange,
    visionRange,
    ageRange,
    polutionMetabolismFactor,
    polutionHarvestFactor,
    diffusePolutionTime,
    childBearingMinAgeRange,
    childBearingFemaleMaxAgeRange,
    childBearingMaleMaxAgeRange,
    sexualReproductionInitialEndowmentRange,
    culturalTagLength,
    combatReward,
    spiceMetabolismRange,
    spiceCapacityRange,
    spiceEndowmentRange,
    spiceGrowbackUnits,
    summerSeasonSpiceGrowbackRatio,
    winterSeasonSpiceGrowbackRatio,
    lendingCreditDuration,
    lendingCreditInterestRate,
    immuneSystemLength,
    diseaseLength,
    diseasesInitial,
    diseasedMetabolismIncrease
  ) where

import FRP.FrABS

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data SugarScapeAgentGender = Male | Female deriving (Show, Eq)
data SugarScapeTribe = Red | Blue deriving (Show, Eq)

type SugarScapeCulturalTag = [Bool]

type SugarScapeCredit = (Double, Double, Double)    -- face-value, duration d years, interest rate r percent
type SugarScapeCreditInfo = (AgentId, Double, SugarScapeCredit)    -- lender id, due age of agent when to pay back credit, credit

type SugarScapeImmuneSystem = [Bool]
type SugarScapeDisease = [Bool]

type MatingReplyTuple = (Double, Double, Double, Int, SugarScapeCulturalTag, SugarScapeImmuneSystem) -- SugarContribution, SugarMetabolism, SpiceMetabolism, Vision

data SugarScapeMsg =
    MatingRequest SugarScapeAgentGender
    | MatingReplyNo
    | MatingReplyYes MatingReplyTuple 
    | MatingChild AgentId
    | MatingChildAck

    | InheritSugar Double

    | CulturalContact SugarScapeCulturalTag

    | KilledInCombat
    
    | TradingOffer Double
    | TradingAccept Double
    | TradingTransact Double
    | TradingRefuse 

    | DiseaseContact SugarScapeDisease 

    | CreditRequest 
    | CreditOffer SugarScapeCredit
    | CreditRequestRefuse 
    | CreditPaybackHalf Double   -- borrower has not enough wealth to fully pay back, will only pay half of its own wealth but credit continues
    | CreditPaybackFull Double   -- full payback this will wipe the credit
    | CreditLenderDied
    | CreditBorrowerDied
    deriving (Show)

data SugarScapeAgentState = SugarScapeAgentState {
    sugAgCoord :: Discrete2dCoord,
    sugAgSugarMetab :: Double,              -- this amount of sugar will be consumed by the agent in each time-step
    sugAgSpiceMetab :: Double,              -- this amount of spice will be consumed by the agent in each time-step

    sugAgVision :: Int,                     -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete

    sugAgSugarLevel :: Double,              -- the current sugar holdings of the agent, if 0 then the agent starves to death
    sugAgSugarInit :: Double,               -- agent is fertile only when its sugarlevel is GE than its initial endowment

    sugAgSpiceLevel :: Double,              -- the current spice holdings of the agent, if 0 then the agent starves to death
    sugAgSpiceInit :: Double,

    sugAgMaxAge :: Double,                  -- at this age the agent will die and create a single new random offspring

    sugAgGender :: SugarScapeAgentGender,   -- an agent is either male or female
    sugAgFertAgeRange :: (Double, Double),   -- an agent younger/older than this cannot bear children

    sugAgChildren :: [AgentId],             -- the ids of all the children this agent has born

    sugAgAge :: Double,                     -- the current age of the agent, could be calculated using time in the SF but we need it in the conversations as well, which are not running in the SF

    sugAgCulturalTag :: SugarScapeCulturalTag,  -- the agents cultural tag
    sugAgTribe :: SugarScapeTribe,           -- the agents tribe it belongs to according to its cultural tag

    sugAgBorrowingCredits :: [SugarScapeCreditInfo],                    -- the agents currently running credits it has to pay back
    sugAgLendingCredits :: [AgentId],                                -- the ids of the borrowers this agent is currently lending to, is necessary to notify borrowers if lender dies

    sugAgImmuneSys :: SugarScapeImmuneSystem,                -- the agents immune-system, a binary string of 0s and 1s
    sugAgImmuneSysBorn :: SugarScapeImmuneSystem,            -- the agents immune-system it is born with, stays constant and is inherited to its children
    sugAgDiseases :: [SugarScapeDisease]                 -- the disease the agent currently has
} deriving (Show)

data SugarScapeEnvCellOccupier = SugarScapeEnvCellOccupier {
    sugEnvOccId :: AgentId,
    sugEnvOccTribe :: SugarScapeTribe,
    sugEnvOccWealth :: Double
} deriving (Show)

data SugarScapeEnvCell = SugarScapeEnvCell {
    sugEnvSugarCapacity :: Double,
    sugEnvSugarLevel :: Double,

    sugEnvSpiceCapacity :: Double,
    sugEnvSpiceLevel :: Double,

    sugEnvPolutionLevel :: Double,
    sugEnvOccupier :: Maybe SugarScapeEnvCellOccupier
} deriving (Show)

type SugarScapeEnvironment = Discrete2d SugarScapeEnvCell
type SugarScapeEnvironmentBehaviour = EnvironmentBehaviour SugarScapeEnvironment
type SugarScapeEnvironmentMonadicBehaviour = EnvironmentMonadicBehaviour SugarScapeEnvironment

type SugarScapeAgentDef = AgentDef SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment
type SugarScapeAgentBehaviour = AgentBehaviour SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment
type SugarScapeAgentIn = AgentIn SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment
type SugarScapeAgentOut = AgentOut SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment
type SugarScapeAgentObservable = AgentObservable SugarScapeAgentState

type SugarScapeAgentConversation = AgentConversationReceiver SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment
type SugarScapeAgentConversationSender = AgentConversationSender SugarScapeAgentState SugarScapeMsg SugarScapeEnvironment

type SugarScapeSimParams = SimulationParams SugarScapeEnvironment
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CONFIGURATION
------------------------------------------------------------------------------------------------------------------------
data AgentColoring = Undefined | Gender | Diseased | Tribe | IdGE Int | VisionGE Int deriving (Eq)

_enablePolution_ :: Bool
_enablePolution_ = False

_enableSpice_ :: Bool
_enableSpice_ = False

_enableBirthAgentOnAgeDeath_ :: Bool
_enableBirthAgentOnAgeDeath_ = False

_enableInheritance_ :: Bool
_enableInheritance_ = False

_enableSeasons_ :: Bool
_enableSeasons_ = False

_enableSex_ :: Bool
_enableSex_ = False

_enableCombat_ :: Bool
_enableCombat_ = False

_enableTrading_ :: Bool
_enableTrading_ = False

_enableCredit_ :: Bool
_enableCredit_ = False

_enableDiseases_ :: Bool
_enableDiseases_ = False

_agentColoring_ :: AgentColoring
_agentColoring_ = Undefined -- Tribe -- VisionGE 4 -- Undefined -- IdGE 401
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- NOTE: < 0 is treated as grow back to max
sugarGrowbackUnits :: Double
sugarGrowbackUnits = 1.0

summerSeasonSugarGrowbackRatio :: Double
summerSeasonSugarGrowbackRatio = 1.0

winterSeasonSugarGrowbackRatio :: Double
winterSeasonSugarGrowbackRatio = 8.0

seasonDuration :: Double
seasonDuration = 50.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (0.0, 4.0)

sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = sugarEndowmentRangeBirthing
-- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
sugarEndowmentRangeStandard = (5.0, 25.0)
-- NOTE: this is specified in book on page 57
sugarEndowmentRangeBirthing = (50.0, 100.0)

sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 5.0)

visionRange :: (Int, Int)
visionRange = visionRangeStandard
-- NOTE: set to 1-6 on page 24
visionRangeStandard = (1, 6)
-- NOTE: for Migration set to 1-10 on page 44
visionRangeSeasons = (1, 10)

ageRange :: (Double, Double)
ageRange = ageRangeInf
ageRangeStandard = (60, 100)
ageRangeInf = (1/0, 1/0)

polutionMetabolismFactor :: Double
polutionMetabolismFactor = 1.0

polutionHarvestFactor :: Double
polutionHarvestFactor = 1.0

diffusePolutionTime :: Int
diffusePolutionTime = 50

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III: Sex, Culture, And Conflict: The Emergence Of History
------------------------------------------------------------------------------------------------------------------------
childBearingMinAgeRange :: (Double, Double)
childBearingMinAgeRange = (12, 15)

childBearingFemaleMaxAgeRange :: (Double, Double)
childBearingFemaleMaxAgeRange = (40, 50)

childBearingMaleMaxAgeRange :: (Double, Double)
childBearingMaleMaxAgeRange = (50, 60)

sexualReproductionInitialEndowmentRange :: (Double, Double)
sexualReproductionInitialEndowmentRange = (50, 100)

culturalTagLength :: Int
culturalTagLength = 10

combatReward :: Double
combatReward = 2.0 -- replace with huge number e.g. 10^7 for harvesting all the other agents sugar 
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER IV: Sugar and Spice - Trade Comes to the Sugarscape
------------------------------------------------------------------------------------------------------------------------
spiceMetabolismRange :: (Double, Double)
spiceMetabolismRange = (1.0, 5.0)

spiceCapacityRange :: (Double, Double)
spiceCapacityRange = (0.0, 4.0)

spiceEndowmentRange :: (Double, Double)
spiceEndowmentRange = (5.0, 25.0)

-- NOTE: < 0 is treated as grow back to max
spiceGrowbackUnits :: Double
spiceGrowbackUnits = 1.0

summerSeasonSpiceGrowbackRatio :: Double
summerSeasonSpiceGrowbackRatio = 1.0

winterSeasonSpiceGrowbackRatio :: Double
winterSeasonSpiceGrowbackRatio = 8.0

lendingCreditDuration :: Double
lendingCreditDuration = 10

-- NOTE: this is the interest-rate in percent
lendingCreditInterestRate :: Double
lendingCreditInterestRate = 2.5

-- NOTE: haven't implemented "On the Evolution of Foresight"
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER V: Disease Processes
------------------------------------------------------------------------------------------------------------------------
immuneSystemLength :: Int
immuneSystemLength = 50

-- NOTE: disease length must be less or equal to immuneSystemLength
diseaseLength :: Int
diseaseLength = 10

diseasesInitial :: Int
diseasesInitial = 25

diseasedMetabolismIncrease :: Double
diseasedMetabolismIncrease = 1.0
------------------------------------------------------------------------------------------------------------------------