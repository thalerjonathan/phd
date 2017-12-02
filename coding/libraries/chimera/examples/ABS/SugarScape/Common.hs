module SugarScape.Common (
    isDiseased,
    metabolismAmount,

    BestCellMeasureFunc,
    selectBestCells,
    bestCellFunc,
    bestMeasureSugarLevel,
    bestMeasureSpiceLevel,
    bestMeasureSugarAndSpiceLevel,
    bestMeasureSugarPolutionRatio,
    bestMeasureSugarAndSpicePolutionRatio,
    bestMeasureSugarAndSpiceLevelWelfareChange,

    unoccupiedNeighbourhoodOfNeighbours,

    excessAmountToChildBearing,
    satisfiesWealthForChildBearing,
    satisfiesWealthForChildBearingM,
    isFertile,
    isFertileM,
    tooOldForChildren,
    withinRange,
    neighbourIds,
    neighbourIdsM,
    createNewBorn,

    filterOccupiers,
    occupierCombatable,
    occupierRetaliator,
    cellPayoff,
    poluteCell,
    agentTradeIncreaseWelfare,
    agentTradeExchange,
    agentMRS,
    sugarSpiceExchange,
    tradingPrice,
    agentWelfare,
    potentialLender,
    isPotentialBorrower,
    agentWelfareChange,
    agentImmunizeAux,

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

import SugarScape.Model

import FRP.FrABS

import FRP.Yampa

import Data.Maybe
import Data.List
import Data.List.Split
import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- GENERAL FUNCTIONS, independent of monadic / non-monadic implementation
------------------------------------------------------------------------------------------------------------------------
type BestCellMeasureFunc = (SugarScapeEnvCell -> Double) 

isDiseased :: SugarScapeAgentState -> Bool
isDiseased = not . null . sugAgDiseases

metabolismAmount :: SugarScapeAgentState -> (Double, Double)
metabolismAmount s = (sugarMetab + inc, spiceMetab + inc)
    where
        sugarMetab = sugAgSugarMetab s
        spiceMetab = sugAgSpiceMetab s
        inc = if isDiseased s then diseasedMetabolismIncrease else 0 

bestCellFunc :: BestCellMeasureFunc
bestCellFunc
    | _enablePolution_ = if _enableSpice_ then bestMeasureSugarAndSpicePolutionRatio else bestMeasureSugarPolutionRatio
    | otherwise = if _enableSpice_ then bestMeasureSugarAndSpiceLevel else bestMeasureSugarLevel

selectBestCells :: BestCellMeasureFunc
                    -> Discrete2dCoord
                    -> [(Discrete2dCoord, SugarScapeEnvCell)]
                    -> [(Discrete2dCoord, SugarScapeEnvCell)]
selectBestCells measureFunc refCoord cs = bestShortestdistanceManhattanCells
    where
        cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
        bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
        bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

        shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d refCoord (fst c1)) (distanceManhattanDisc2d refCoord (fst c2))) bestCells
        shortestdistanceManhattan = distanceManhattanDisc2d refCoord (fst $ head shortestdistanceManhattanBestCells)
        bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d refCoord) . fst) shortestdistanceManhattanBestCells

unoccupiedNeighbourhoodOfNeighbours :: Discrete2dCoord -> SugarScapeEnvironment -> [(Discrete2dCoord, SugarScapeEnvCell)]
unoccupiedNeighbourhoodOfNeighbours coord e = filter (isNothing . sugEnvOccupier . snd) nncsUnique
    where
        neighbourCells = neighbours coord False e
        -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
        nncsDupl = foldr (\(coord, _) acc -> neighbours coord False e ++ acc) neighbourCells neighbourCells
        -- NOTE: the nncs are not unique, remove duplicates
        nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl

bestMeasureSugarLevel :: BestCellMeasureFunc
bestMeasureSugarLevel c = sugEnvSugarLevel c

bestMeasureSpiceLevel :: BestCellMeasureFunc
bestMeasureSpiceLevel c = sugEnvSpiceLevel c

bestMeasureSugarAndSpiceLevel :: BestCellMeasureFunc
bestMeasureSugarAndSpiceLevel c = sugLvl + spiLvl
    where
        sugLvl = sugEnvSugarLevel c
        spiLvl = sugEnvSugarLevel c

bestMeasureSugarPolutionRatio :: BestCellMeasureFunc
bestMeasureSugarPolutionRatio c = sugLvl / (1 + polLvl)
    where
        sugLvl = sugEnvSugarLevel c
        polLvl = sugEnvPolutionLevel c

bestMeasureSugarAndSpicePolutionRatio :: BestCellMeasureFunc
bestMeasureSugarAndSpicePolutionRatio c = (sugLvl + spiLvl) / (1 + polLvl)
    where
        sugLvl = sugEnvSugarLevel c
        spiLvl = sugEnvSugarLevel c
        polLvl = sugEnvPolutionLevel c

bestMeasureSugarAndSpiceLevelWelfareChange :: SugarScapeAgentState -> BestCellMeasureFunc
bestMeasureSugarAndSpiceLevelWelfareChange s c = agentWelfareChange s (sg, sp) -- TODO: why is there welfarechange included? need to give a different name
    where
        sg = sugEnvSugarLevel c
        sp = sugEnvSpiceLevel c

excessAmountToChildBearing :: SugarScapeAgentState -> Double
excessAmountToChildBearing s = currSugar - initSugar
    where
        currSugar = sugAgSugarLevel s
        initSugar = sugAgSugarInit s

satisfiesWealthForChildBearing :: SugarScapeAgentState -> Bool
satisfiesWealthForChildBearing s = excessAmount >= 0
    where
        excessAmount = excessAmountToChildBearing s

satisfiesWealthForChildBearingM ::State SugarScapeAgentOut Bool
satisfiesWealthForChildBearingM = state (\ao -> (satisfiesWealthForChildBearing $ agentState ao, ao))

isFertile :: SugarScapeAgentState -> Bool
isFertile s = withinRange age fertilityAgeRange
    where
        age = sugAgAge s
        fertilityAgeRange = sugAgFertAgeRange s

isFertileM :: State SugarScapeAgentOut Bool
isFertileM = state (\ao -> (isFertile $ agentState ao, ao))

tooOldForChildren :: SugarScapeAgentState -> Bool
tooOldForChildren s = age > fertilityAgeMax 
    where
        age = sugAgAge s
        (_, fertilityAgeMax) = sugAgFertAgeRange s

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

neighbourIds :: SugarScapeEnvironment -> SugarScapeAgentOut -> [AgentId]
neighbourIds e ao = map (sugEnvOccId . fromJust . sugEnvOccupier) occupiedCells
    where
        coord = sugAgCoord $ agentState ao
        ncs = neighbourCells coord False e -- NOTE: this includes only neighbours, never self, never required in this function
        occupiedCells = filter (isJust . sugEnvOccupier) ncs

neighbourIdsM :: SugarScapeEnvironment -> State SugarScapeAgentOut [AgentId]
neighbourIdsM e = state (\ao -> (neighbourIds e ao, ao))

createNewBorn :: (AgentId, Discrete2dCoord)
                    -> (Double, Double, Double, Int, SugarScapeCulturalTag, SugarScapeImmuneSystem)
                    -> (Double, Double, Double, Int, SugarScapeCulturalTag, SugarScapeImmuneSystem)
                    -> SugarScapeAgentBehaviour
                    -> SugarScapeAgentConversation
                    -> Rand StdGen SugarScapeAgentDef
createNewBorn idCoord
                (sugEndowFather, sugarMetabFather, spiceMetabFather, visionFather, cultureFather, immuneSysBornFather)
                (sugEndowMother, sugarMetabMother, spiceMetabMother, visionMother, cultureMother, immuneSysBornMother)
                beh
                conv =
    do
        newBornSugarMetab <- crossover (sugarMetabFather, sugarMetabMother)
        newBornSpiceMetab <- crossover (spiceMetabFather, spiceMetabMother)
        newBornVision <- crossover (visionFather, visionMother)
        newBornCulturalTag <- crossoverBools cultureFather cultureMother
        newBornImmuneSystem <- crossoverBools immuneSysBornFather immuneSysBornMother

        let newBornSugarEndow = sugEndowFather + sugEndowMother

        newBornDef <- randomAgent
                            idCoord
                            beh
                            conv

        let newBornState' = (adState newBornDef) { sugAgSugarMetab = newBornSugarMetab,
                                                   sugAgSpiceMetab = newBornSpiceMetab,
                                                   sugAgVision = newBornVision,
                                                   sugAgSugarLevel = newBornSugarEndow,
                                                   sugAgSugarInit = newBornSugarEndow,
                                                   sugAgCulturalTag = newBornCulturalTag,
                                                   sugAgTribe = calculateTribe newBornCulturalTag,
                                                   sugAgImmuneSys = newBornImmuneSystem,
                                                   sugAgImmuneSysBorn = newBornImmuneSystem,
                                                   sugAgDiseases = [] }

        return newBornDef { adState = newBornState' }

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER III
------------------------------------------------------------------------------------------------------------------------
filterOccupiers :: (SugarScapeEnvCellOccupier -> Bool) -> [(Discrete2dCoord, SugarScapeEnvCell)] -> [(Discrete2dCoord, SugarScapeEnvCell)] 
filterOccupiers f cs = filter filterOccupiersAux cs
    where
        filterOccupiersAux :: (Discrete2dCoord, SugarScapeEnvCell) -> Bool
        filterOccupiersAux (_, cell) = maybe True f (sugEnvOccupier cell)

occupierCombatable :: Double
                        -> SugarScapeTribe 
                        -> SugarScapeEnvCellOccupier
                        -> Bool
occupierCombatable myWealth myTribe occupier = differentTribe && lessWealthy
    where
        otherTribe = sugEnvOccTribe occupier
        otherWealth = sugEnvOccWealth occupier
        differentTribe = otherTribe /= myTribe
        lessWealthy = otherWealth < myWealth 

occupierRetaliator :: Double 
                        -> SugarScapeTribe 
                        -> SugarScapeEnvCellOccupier
                        -> Bool
occupierRetaliator referenceWealth myTribe occupier = differentTribe && moreWealthy
    where
        otherTribe = sugEnvOccTribe occupier
        otherWealth = sugEnvOccWealth occupier
        differentTribe = otherTribe /= myTribe
        moreWealthy = otherWealth > referenceWealth 

cellPayoff :: (Discrete2dCoord, SugarScapeEnvCell) -> ((Discrete2dCoord, SugarScapeEnvCell), Double)
cellPayoff (c, cell) = ((c, cell), payoff)
    where
        mayOccupier = sugEnvOccupier cell
        sugarLevel = sugEnvSugarLevel cell
        payoff = maybe sugarLevel (\occupier -> sugarLevel + (min combatReward (sugEnvOccWealth occupier))) mayOccupier

poluteCell :: Double -> Discrete2dCoord -> SugarScapeEnvironment -> SugarScapeEnvironment
poluteCell polutionIncrease coord e 
    | _enablePolution_ = changeCellAt coord cellAfterPolution e
    | otherwise = e
    where
        cell = cellAt coord e
        cellAfterPolution = cell { sugEnvPolutionLevel = polutionIncrease + sugEnvPolutionLevel cell }
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER IV
------------------------------------------------------------------------------------------------------------------------
agentTradeIncreaseWelfare :: SugarScapeAgentState -> Double -> Bool
agentTradeIncreaseWelfare s mrsOther = (newWelfare > currentWelfare)
    where
        mrsSelf = agentMRS s
        exchangeTup = sugarSpiceExchange mrsOther mrsSelf
        currentWelfare = agentWelfare s
        newWelfare = agentWelfareChange s exchangeTup

agentTradeExchange :: SugarScapeAgentState -> Double -> SugarScapeAgentState
agentTradeExchange s mrsOther = sAfterTrade -- trace ("Trade: mrsSelf = " ++ (show mrsSelf) ++ " mrsOther = " ++ (show mrsOther) ++ ", sugarChange = " ++ (show sugarChange) ++ " spiceChange = " ++ (show spiceChange) ) 
    where
        mrsSelf = agentMRS s
        (sugarChange, spiceChange) = sugarSpiceExchange mrsOther mrsSelf

        sugarLevel = sugAgSugarLevel s
        spiceLevel = sugAgSpiceLevel s

        sAfterTrade  = s { sugAgSugarLevel = sugarLevel + sugarChange, sugAgSpiceLevel = spiceLevel + spiceChange }
 
agentMRS :: SugarScapeAgentState -> Double
agentMRS s = (w2 * m1) / (w1 * m2)
    where
        m1 = sugAgSugarMetab s
        m2 = sugAgSpiceMetab s

        w1 = sugAgSugarLevel s
        w2 = sugAgSpiceLevel s

-- NOTE: this returns the sugar-to-spice exchanges from the view-point of self
sugarSpiceExchange :: Double -> Double -> (Double, Double)
sugarSpiceExchange mrsOther mrsSelf 
    -- NOTE: if mrsOther is larger than mrsSelf then Other values sugar more and is willing to exchange it for spice
        -- Other takes (+) sugar and gives spice
        -- Self takes (+) spice and gives (-) sugar
    | (mrsOther > mrsSelf) && (price > 1) = ((-1.0), price)
    | (mrsOther > mrsSelf) && (price <= 1) = ((-invPrice), 1.0)

    -- NOTE: if mrsSelf is larger than mrsOther then Self values sugar more and is willing to exchange it for spice
        -- Self takes sugar and gives spice
        -- Other takes spice and gives sugar
    | (mrsOther <= mrsSelf) && (price > 1) = (1.0, (-price)) 
    | (mrsOther <= mrsSelf) && (price <= 1) = (invPrice, (-1.0))
    where
        price = tradingPrice mrsOther mrsSelf
        invPrice = 1 / price

tradingPrice :: Double -> Double -> Double
tradingPrice mrsA mrsB = sqrt $ mrsA * mrsB

agentWelfare :: SugarScapeAgentState -> Double
agentWelfare s = agentWelfareChange s (0, 0)

potentialLender :: SugarScapeAgentState -> Maybe Double
potentialLender s
    | tooOldForChildren s = Just $ half (sugAgSugarLevel s)
    | isFertile s = fertileLending s
    | otherwise = Nothing
    where
        fertileLending :: SugarScapeAgentState -> Maybe Double
        fertileLending s 
            | excessAmount > 0 = Just excessAmount
            | otherwise = Nothing
            where
                excessAmount = excessAmountToChildBearing s

        half x = x * 0.5

isPotentialBorrower :: SugarScapeAgentState -> Bool
isPotentialBorrower s 
    | isFertile s && (not $ satisfiesWealthForChildBearing s) = True
    | otherwise = False


agentWelfareChange :: SugarScapeAgentState -> (Double, Double) -> Double
agentWelfareChange s (sugarChange, spiceChange) = ((w1 + sugarChange)**(m1/mT)) * ((w2 + spiceChange)**(m2/mT))
    where
        m1 = sugAgSugarMetab s
        m2 = sugAgSpiceMetab s
        mT = m1 + m2

        w1 = sugAgSugarLevel s
        w2 = sugAgSpiceLevel s
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER V
------------------------------------------------------------------------------------------------------------------------
agentImmunizeAux :: SugarScapeDisease 
                    -> (SugarScapeImmuneSystem, [SugarScapeDisease]) 
                    -> (SugarScapeImmuneSystem, [SugarScapeDisease])
agentImmunizeAux disease (imSys, accDis) 
    | minHam == 0 = (imSys, accDis)
    | otherwise = (imSys', disease : accDis)
    where
        dLen = length disease

        hd = calculateHammingDistances imSys disease
        mi@(minHam, minHamIdx) = findMinWithIdx hd

        minSubImmSys = take dLen (drop minHamIdx imSys)

        tagIdx = findFirstDiffIdx minSubImmSys disease
        globalIdx = minHamIdx + tagIdx
        imSys' = flipBoolAtIdx imSys globalIdx
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- UTILS
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
        randIdx <- getRandomR (0, tagLength - 1)
        return $ flipCulturalTag tagActive tagPassive randIdx
    where
        tagLength = length tagActive
        
-- NOTE: the tags must have same length, this could be enforced statically through types if we had a dependent type-system
flipCulturalTag :: SugarScapeCulturalTag 
                    -> SugarScapeCulturalTag 
                    -> Int 
                    -> SugarScapeCulturalTag
flipCulturalTag [] [] _ = []
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
------------------------------------------------------------------------------------------------------------------------

randomAgent :: (AgentId, Discrete2dCoord)
                -> SugarScapeAgentBehaviour
                -> SugarScapeAgentConversation
                -> Rand StdGen SugarScapeAgentDef
randomAgent (agentId, coord) beh conv = 
    do
        -- NOTE: need to split here otherwise agents would end up with the same random-values when not already splitting in the calling function
        rng <- getSplit

        randSugarMetab <- getRandomR sugarMetabolismRange
        randSpiceMetab <- getRandomR spiceMetabolismRange
        randVision <- getRandomR  visionRange
        randSugarEndowment <- getRandomR sugarEndowmentRange
        randSpiceEndowment <- getRandomR spiceEndowmentRange
        randMaxAge <- getRandomR ageRange
        randMale <- getRandomR (True, False)
        randMinFert <- getRandomR childBearingMinAgeRange
        randCulturalTagInf <- getRandoms 
        randImmuneSystemInf <- getRandoms
        randDiseasesInitialInf <- getRandoms 

        let randCulturalTag = take culturalTagLength randCulturalTagInf
        let randImmuneSystem = take immuneSystemLength randImmuneSystemInf

        let diseasesStrLen = (diseaseLength * diseasesInitial) :: Int
        let randDiseasesInitialStr = take diseasesStrLen randDiseasesInitialInf :: [Bool]
        let randDiseasesInitialChunks = Data.List.Split.chunksOf diseaseLength randDiseasesInitialStr 

        let randGender = if randMale then Male else Female
        let fertilityMaxRange = if randMale then childBearingMaleMaxAgeRange else childBearingFemaleMaxAgeRange

        randMaxFert <- getRandomR fertilityMaxRange
        
        let spiceMetab = if _enableSpice_ then randSpiceMetab else 0
        let spiceEndow = if _enableSpice_ then randSpiceEndowment else 0

        let immuneSys = if _enableDiseases_ then randImmuneSystem else []
        let diseases = if _enableDiseases_ then randDiseasesInitialChunks else []
        
        let s = SugarScapeAgentState {
            sugAgCoord = coord,

            sugAgSugarMetab = randSugarMetab,
            sugAgSpiceMetab = spiceMetab,

            sugAgVision = randVision,

            sugAgSugarLevel = randSugarEndowment,
            sugAgSugarInit = randSugarEndowment,

            sugAgSpiceInit = spiceEndow,
            sugAgSpiceLevel = spiceEndow,

            sugAgMaxAge = randMaxAge,

            sugAgGender = randGender,
            sugAgFertAgeRange = (randMinFert, randMaxFert),

            sugAgChildren = [],

            sugAgAge = 0.0,

            sugAgCulturalTag = randCulturalTag,
            sugAgTribe = calculateTribe randCulturalTag,

            sugAgBorrowingCredits = [],
            sugAgLendingCredits = [],

            sugAgImmuneSys = immuneSys,
            sugAgImmuneSysBorn = immuneSys,
            sugAgDiseases = diseases
        }

        let adef = AgentDef {
           adId = agentId,
           adState = s,
           adConversation = Just conv,
           adInitMessages = NoEvent,
           adBeh = beh,
           adRng = rng }

        return adef