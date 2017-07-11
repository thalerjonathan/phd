module SugarScape.Model (
    SugarScapeAgentGender (..),
    SugarScapeTribe (..),
    SugarScapeCulturalTag,
    SugarScapeCredit,
    SugarScapeCreditInfo,
    SugarScapeImmuneSystem,
    SugarScapeDisease,
    SugarScapeMsg (..),

    SugarScapeAgentState (..),

    SugarScapeEnvCellOccupier (..),
    SugarScapeEnvCell (..),

    SugarScapeEnvLink,
    SugarScapeEnvironment,
    SugarScapeEnvironmentBehaviour,

    SugarScapeAgentDef,
    SugarScapeAgentBehaviour,
    SugarScapeAgentIn,
    SugarScapeAgentOut,

    SugarScapeAgentConversation,
    SugarScapeAgentConversationSender,
    SugarScapeSimParams,

    sugarGrowbackUnits,
    summerSeasonSugarGrowbackRate, 
    winterSeasonSugarGrowbackRate,
    seasonDuration,
    sugarCapacityRange,
    sugarEndowmentRange,
    sugarMetabolismRange,
    visionRange,
    ageRange,
    polutionEnabled,
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
    summerSeasonSpiceGrowbackRate,
    winterSeasonSpiceGrowbackRate,
    lendingCreditDuration,
    lendingCreditInterestRate,
    immuneSystemLength,
    diseaseLength,
    diseasesInitial,
    diseasedMetabolismIncrease,

    cellOccupier,
    calculateTribe,
    cultureContact,
    flipCulturalTag,
    crossoverBools,
    crossover,
    flipBoolAtIdx,
    findFirstDiffIdx,
    findMinWithIdx,
    calculateHammingDistances,
    hammingDistance,
    randomAgent
  ) where

import FRP.FrABS

import FRP.Yampa

import System.Random
import Control.Monad.Random
import Data.List.Split
import Data.List
import Data.Maybe

-- TODO: when sex is turned on the number of agents is constantly increasing which should not be possible because more agents compete for less ressources which should reduce the population. Probably we are leaking wealth

-- TODO: export dynamics in a text file with matlab format of the data: wealth distribution, number of agents, mean vision/metabolism, mean age,

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

data SugarScapeMsg =
    MatingRequest SugarScapeAgentGender
    | MatingReplyNo
    | MatingReplyYes (Double, Double, Double, Int, SugarScapeCulturalTag, SugarScapeImmuneSystem) -- SugarContribution, SugarMetabolism, SpiceMetabolism, Vision
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

type SugarScapeEnvLink = ()
type SugarScapeEnvironment = Environment SugarScapeEnvCell SugarScapeEnvLink
type SugarScapeEnvironmentBehaviour = EnvironmentBehaviour SugarScapeEnvCell SugarScapeEnvLink

type SugarScapeAgentDef = AgentDef SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink
type SugarScapeAgentBehaviour = AgentBehaviour SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink
type SugarScapeAgentIn = AgentIn SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink
type SugarScapeAgentOut = AgentOut SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink

type SugarScapeAgentConversation = AgentConversationReceiver SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink
type SugarScapeAgentConversationSender = AgentConversationSender SugarScapeAgentState SugarScapeMsg SugarScapeEnvCell SugarScapeEnvLink

type SugarScapeSimParams = SimulationParams SugarScapeEnvCell SugarScapeEnvLink
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
sugarGrowbackUnits :: Double
sugarGrowbackUnits = 1.0

summerSeasonSugarGrowbackRate :: Double
summerSeasonSugarGrowbackRate = 1.0

winterSeasonSugarGrowbackRate :: Double
winterSeasonSugarGrowbackRate = 8.0

seasonDuration :: Double
seasonDuration = 10.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (0.0, 4.0)

-- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = (5.0, 25.0)

sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 5.0)

visionRange :: (Int, Int)
visionRange = (1, 10)

ageRange :: (Double, Double)
ageRange = (60, 100)


polutionEnabled :: Bool
polutionEnabled = False

polutionMetabolismFactor :: Double
polutionMetabolismFactor = 1.0

polutionHarvestFactor :: Double
polutionHarvestFactor = 1.0

diffusePolutionTime :: Int
diffusePolutionTime = 12

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

spiceGrowbackUnits :: Double
spiceGrowbackUnits = 1.0

summerSeasonSpiceGrowbackRate :: Double
summerSeasonSpiceGrowbackRate = 1.0

winterSeasonSpiceGrowbackRate :: Double
winterSeasonSpiceGrowbackRate = 8.0

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
diseasesInitial = 4

diseasedMetabolismIncrease :: Double
diseasedMetabolismIncrease = 1.0
------------------------------------------------------------------------------------------------------------------------

cellOccupier :: AgentId -> SugarScapeAgentState -> SugarScapeEnvCellOccupier
cellOccupier aid s = SugarScapeEnvCellOccupier {
                        sugEnvOccId = aid,
                        sugEnvOccTribe = sugAgTribe s,
                        sugEnvOccWealth = wealth
                    }
    where
        wealth = (sugAgSugarLevel s) + (sugAgSpiceLevel s)

calculateTribe :: SugarScapeCulturalTag -> SugarScapeTribe
calculateTribe tag
    | falseCount >= trueCount = Blue
    | otherwise = Red
    where
        falseCount = length $ filter (==False) tag 
        trueCount = length $ filter (==True) tag 
       
-- NOTE: the tags must have same length, this could be enforced statically through types if we had a dependent type-system
cultureContact :: SugarScapeCulturalTag 
                    -> SugarScapeCulturalTag 
                    -> Rand StdGen SugarScapeCulturalTag
cultureContact tagActive tagPassive = 
    do
        randIdx <- getRandomR (0, tagLength-1)
        return $ flipCulturalTag tagActive tagPassive randIdx
    where
        tagLength = length tagActive
        
-- NOTE: the tags must have same length, this could be enforced statically through types if we had a dependent type-system
flipCulturalTag :: SugarScapeCulturalTag 
                    -> SugarScapeCulturalTag 
                    -> Int 
                    -> SugarScapeCulturalTag
flipCulturalTag tagActive tagPassive idx = map (\(i, a, p) -> if i == idx then a else p) (zip3 [0..len-1] tagActive tagPassive) 
    where
        len = length tagActive

crossoverBools :: [Bool] 
                    -> [Bool]  
                    -> Rand StdGen [Bool] 
crossoverBools ts1 ts2 = 
    do
        randTags <- replicateM (length ts1) (getRandomR (True, False))
        return $ map (\(t1, t2, randT) -> if t1 == t2 then t1 else randT) (zip3 ts1 ts2 randTags)

crossover :: (a, a) -> Rand StdGen a
crossover (x, y) =
    do
        takeX <- getRandomR (True, False)
        if takeX then
            return x
            else
                return y


flipBoolAtIdx :: [Bool] -> Int -> [Bool]
flipBoolAtIdx as idx = front ++ (flippedElem : backNoElem)
    where
        (front, back) = splitAt idx as  -- NOTE: back includes the element with the index
        elemAtIdx = as !! idx
        flippedElem = not elemAtIdx
        backNoElem = tail back

findFirstDiffIdx :: (Eq a) => [a] -> [a] -> (Int)
findFirstDiffIdx as bs = firstNotEqualIdx
    where
        notEquals = map (\(a, b) -> a /= b) (zip as bs)
        firstNotEqualIdx = fromJust $ findIndex (==True) notEquals

findMinWithIdx :: (Ord a) => [a] -> (a, Int)
findMinWithIdx as = (minA, minAIdx)
    where
        minA = minimum as
        minAIdx = fromJust $ findIndex (==minA) as

calculateHammingDistances :: [Bool] -> [Bool] -> [Int]
calculateHammingDistances i d = map (\is -> hammingDistance is d) isubs
    where
        dLen = length d
        isubs = Data.List.Split.divvy dLen 1 i

-- NOTE: both must have the same length
hammingDistance :: [Bool] -> [Bool] -> Int
hammingDistance as bs = length $ filter (==False) equals
    where
        equals = map (\(a, b) -> a == b) (zip as bs)


randomAgent :: (AgentId, EnvCoord)
                -> SugarScapeAgentBehaviour
                -> SugarScapeAgentConversation
                -> Rand StdGen SugarScapeAgentDef
randomAgent (agentId, coord) beh conv = 
    do
        randSugarMetab <- getRandomR sugarMetabolismRange
        randSpiceMetab <- getRandomR spiceMetabolismRange
        randVision <- getRandomR  visionRange
        randSugarEndowment <- getRandomR sugarEndowmentRange
        randSpiceEndowment <- getRandomR spiceEndowmentRange
        -- randSugarEndowment <- getRandomR sexualReproductionInitialEndowmentRange
        randMaxAge <- getRandomR  ageRange
        randMale <- getRandomR (True, False)
        randMinFert <- getRandomR childBearingMinAgeRange
        randCulturalTagInf <- getRandoms 
        randImmuneSystemInf <- getRandoms
        randDiseasesInitialInf <- getRandoms 

        let randCulturalTag = take culturalTagLength randCulturalTagInf
        let randImmuneSystem = take immuneSystemLength randImmuneSystemInf

        let diseasesStrLen = (diseaseLength * diseasesInitial) :: Int
        let randDiseasesInitialStr = take diseasesStrLen randDiseasesInitialInf
        let randDiseasesInitialChunks = Data.List.Split.chunksOf diseaseLength randDiseasesInitialStr

        let randGender = if randMale then Male else Female
        let fertilityMaxRange = if randMale then childBearingMaleMaxAgeRange else childBearingFemaleMaxAgeRange

        randMaxFert <- getRandomR fertilityMaxRange

        rng <- getSplit

        let s = SugarScapeAgentState {
            sugAgSugarMetab = randSugarMetab,
            sugAgSpiceMetab = randSpiceMetab,

            sugAgVision = randVision,

            sugAgSugarLevel = randSugarEndowment,
            sugAgSugarInit = randSugarEndowment,

            sugAgSpiceInit = randSpiceEndowment,
            sugAgSpiceLevel = randSpiceEndowment,

            sugAgMaxAge = randMaxAge,

            sugAgGender = randGender,
            sugAgFertAgeRange = (randMinFert, randMaxFert),

            sugAgChildren = [],

            sugAgAge = 0.0,

            sugAgCulturalTag = randCulturalTag,
            sugAgTribe = calculateTribe randCulturalTag,

            sugAgBorrowingCredits = [],
            sugAgLendingCredits = [],

            sugAgImmuneSys = randImmuneSystem,
            sugAgImmuneSysBorn = randImmuneSystem,
            sugAgDiseases = randDiseasesInitialChunks
        }

        let adef = AgentDef {
           adId = agentId,
           adState = s,
           adEnvPos = coord,
           adConversation = Just conv,
           adInitMessages = NoEvent,
           adBeh = beh,
           adRng = rng }

        return adef