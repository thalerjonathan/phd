{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Model 
  (
    AgentId
    
  , SugAgentGender (..)
  , SugTribe (..)
  , SugCulturalTag
  , SugCredit
  , SugCreditInfo
  , SugImmuneSystem
  , SugDisease
  , SugEvt (..)
  , MatingReplyTuple

  , SugAgentState (..)
  , SugAgentObservable (..)

  , SugEnvCellOccupier (..)
  , SugEnvCell (..)

  , SugEnvironment

  , SugContext (..)
  , SugAgent
  , SugAgentMonad

  , SugAgentIn (..)
  , SugAgentOut (..)

  , nextAgentId
  , readEnvironment
  , writeEnvironment

  , AgentColoring (..)
  , _enablePolution_
  , _enableSpice_
  , _enableBirthAgentOnAgeDeath_
  , _enableInheritance_
  , _enableSex_
  , _enableCombat_
  , _enableDiseases_
  , _enableSeasons_
  , _enableTrading_
  , _enableCredit_
  , _agentColoring_

  , sugarGrowbackUnits
  , summerSeasonSugarGrowbackRatio
  , winterSeasonSugarGrowbackRatio
  , seasonDuration
  , sugarCapacityRange
  , sugarEndowmentRange
  , sugarEndowmentRangeStandard
  , sugarEndowmentRangeBirthing
  , sugarMetabolismRange
  , visionRange
  , visionRangeStandard
  , visionRangeSeasons
  , ageRange
  , ageRangeStandard
  , ageRangeInf
  , polutionMetabolismFactor
  , polutionHarvestFactor
  , diffusePolutionTime
  , childBearingMinAgeRange
  , childBearingFemaleMaxAgeRange
  , childBearingMaleMaxAgeRange
  , sexualReproductionInitialEndowmentRange
  , culturalTagLength
  , combatReward
  , spiceMetabolismRange
  , spiceCapacityRange
  , spiceEndowmentRange
  , spiceGrowbackUnits
  , summerSeasonSpiceGrowbackRatio
  , winterSeasonSpiceGrowbackRatio
  , lendingCreditDuration
  , lendingCreditInterestRate
  , immuneSystemLength
  , diseaseLength
  , diseasesInitial
  , diseasedMetabolismIncrease
  ) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Concurrent.STM
import FRP.BearRiver

import Discrete

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type AgentId = Int

data SugAgentGender 
  = Male 
  | Female deriving (Show, Eq)

data SugTribe
  = Red 
  | Blue deriving (Show, Eq)

type SugCulturalTag = [Bool]

type SugCredit = (Double, Double, Double)    -- face-value, duration d years, interest rate r percent
type SugCreditInfo = (AgentId, Double, SugCredit)    -- lender id, due age of agent when to pay back credit, credit

type SugImmuneSystem = [Bool]
type SugDisease = [Bool]

type MatingReplyTuple = (Double, Double, Double, Int, SugCulturalTag, SugImmuneSystem) -- SugarContribution, SugarMetabolism, SpiceMetabolism, Vision

data SugEvt
  = MatingRequest SugAgentGender
  | MatingReplyNo
  | MatingReplyYes MatingReplyTuple 
  | MatingChild AgentId
  | MatingChildAck

  | InheritSugar Double

  | CulturalContact SugCulturalTag

  | KilledInCombat
  
  | TradingOffer Double
  | TradingAccept Double
  | TradingTransact Double
  | TradingRefuse 

  | DiseaseContact SugDisease 

  | CreditRequest 
  | CreditOffer SugCredit
  | CreditRequestRefuse 
  | CreditPaybackHalf Double   -- borrower has not enough wealth to fully pay back, will only pay half of its own wealth but credit continues
  | CreditPaybackFull Double   -- full payback this will wipe the credit
  | CreditLenderDied
  | CreditBorrowerDied
  deriving (Show)

data SugAgentState = SugAgentState 
  { sugAgCoord            :: Discrete2dCoord
  , sugAgSugarMetab       :: Double              -- this amount of sugar will be consumed by the agent in each time-step
  , sugAgSpiceMetab       :: Double              -- this amount of spice will be consumed by the agent in each time-step
  , sugAgVision           :: Int                 -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete
  , sugAgSugarLevel       :: Double              -- the current sugar holdings of the agent, if 0 then the agent starves to death
  , sugAgSugarInit        :: Double              -- agent is fertile only when its sugarlevel is GE than its initial endowment
  , sugAgSpiceLevel       :: Double              -- the current spice holdings of the agent, if 0 then the agent starves to death
  , sugAgSpiceInit        :: Double
  , sugAgMaxAge           :: Double                  -- at this age the agent will die and create a single new random offspring
  , sugAgGender           :: SugAgentGender   -- an agent is either male or female
  , sugAgFertAgeRange     :: (Double, Double)   -- an agent younger/older than this cannot bear children
  , sugAgChildren         :: [AgentId]             -- the ids of all the children this agent has born
  , sugAgAge              :: Double                     -- the current age of the agent, could be calculated using time in the SF but we need it in the conversations as well, which are not running in the SF
  , sugAgCulturalTag      :: SugCulturalTag  -- the agents cultural tag
  , sugAgTribe            :: SugTribe           -- the agents tribe it belongs to according to its cultural tag
  , sugAgBorrowingCredits :: [SugCreditInfo]                    -- the agents currently running credits it has to pay back
  , sugAgLendingCredits   :: [AgentId]                                -- the ids of the borrowers this agent is currently lending to, is necessary to notify borrowers if lender dies
  , sugAgImmuneSys        :: SugImmuneSystem                -- the agents immune-system, a binary string of 0s and 1s
  , sugAgImmuneSysBorn    :: SugImmuneSystem            -- the agents immune-system it is born with, stays constant and is inherited to its children
  , sugAgDiseases         :: [SugDisease]                 -- the disease the agent currently has
  } deriving (Show)

data SugAgentObservable = SugAgentObservable
  {
    sugObsCoord    :: Discrete2dCoord
  , sugObsGender   :: SugAgentGender
  , sugObsVision   :: Int
  , sugObsDiseases :: [SugDisease] 
  , sugObsTribe    :: SugTribe
  } deriving (Show)

data SugEnvCellOccupier = SugEnvCellOccupier 
  { sugEnvOccId     :: AgentId
  , sugEnvOccTribe  :: SugTribe
  , sugEnvOccWealth :: Double
  } deriving (Show)

data SugEnvCell = SugEnvCell 
  { sugEnvSugarCapacity :: Double
  , sugEnvSugarLevel    :: Double
  , sugEnvSpiceCapacity :: Double
  , sugEnvSpiceLevel    :: Double
  , sugEnvPolutionLevel :: Double
  , sugEnvOccupier      :: Maybe SugEnvCellOccupier
  } deriving (Show)

-- TODO: to reduce STM retries use TArray for Environment
type SugEnvironment = Discrete2d SugEnvCell

data SugAgentIn = SugAgentIn { }

data SugAgentOut g = SugAgentOut 
  { sugAoKill       :: !(Event ())
  , sugAoNew        :: ![(AgentId, SugAgent g)]
  , sugAoObservable :: !(Maybe SugAgentObservable)
  }

data SugContext = SugContext 
  { sugCtxEnv     :: TVar SugEnvironment
  , sugCtxNextAid :: TVar AgentId
  }

type SugAgentMonad g = ReaderT SugContext (RandT g STM)
type SugAgent g      = SF (SugAgentMonad g) SugAgentIn (SugAgentOut g)

nextAgentId :: RandomGen g
            => (SugAgentMonad g) AgentId
nextAgentId = do
  ctx <- ask
  let aidVar = sugCtxNextAid ctx

  aid <- lift $ lift $ readTVar aidVar
  _   <- lift $ lift $ writeTVar aidVar (aid + 1)

  return aid
  
readEnvironment :: RandomGen g 
                => (SugAgentMonad g) SugEnvironment
readEnvironment = do
  ctx <- ask
  let envVar = sugCtxEnv ctx
  lift $ lift $ readTVar envVar

writeEnvironment :: RandomGen g 
                => SugEnvironment
                -> (SugAgentMonad g) ()
writeEnvironment e = do
  ctx <- ask
  let envVar = sugCtxEnv ctx
  lift $ lift $ writeTVar envVar e
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CONFIGURATION
------------------------------------------------------------------------------------------------------------------------
data AgentColoring 
  = Undefined 
  | Gender 
  | Diseased 
  | Tribe 
  | IdGE Int 
  | VisionGE Int deriving (Eq)

_enablePolution_ :: Bool
_enablePolution_ = False

_enableSpice_ :: Bool
_enableSpice_ = False

_enableBirthAgentOnAgeDeath_ :: Bool
_enableBirthAgentOnAgeDeath_ = False

_enableInheritance_ :: Bool
_enableInheritance_ = False

_enableSeasons_ :: Bool
_enableSeasons_ = True

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
sugarEndowmentRangeStandard :: (Double, Double)
sugarEndowmentRangeStandard = (5.0, 25.0)
-- NOTE: this is specified in book on page 57
sugarEndowmentRangeBirthing :: (Double, Double)
sugarEndowmentRangeBirthing = (50.0, 100.0)

sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 5.0)

visionRange :: (Int, Int)
visionRange = visionRangeStandard
-- NOTE: set to 1-6 on page 24
visionRangeStandard :: (Int, Int)
visionRangeStandard = (1, 6)
-- NOTE: for Migration set to 1-10 on page 44
visionRangeSeasons :: (Int, Int)
visionRangeSeasons = (1, 10)

ageRange :: (Double, Double)
ageRange = ageRangeStandard
ageRangeStandard :: (Double, Double)
ageRangeStandard = (60, 100)
ageRangeInf :: (Double, Double)
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