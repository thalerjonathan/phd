module SugarScape.AgentCommon (
    isDiseased,
    metabolismAmount,

    BestCellMeasureFunc,
    selectBestCells,
    bestMeasureSugarLevel,
    bestMeasureSugarAndSpiceLevel,
    bestMeasureSugarPolutionRatio,
    excessAmountToChildBearing,
    satisfiesWealthForChildBearing,
    isFertile,
    tooOldForChildren,
    withinRange,
    neighbourIds,
    neighbourIdsM,
    createNewBorn,

    filterTargetCell,
    occupierCombatable,
    occupierRetaliator,
    cellPayoff,
    agentTradeIncreaseWelfare,
    agentTradeExchange,
    agentMRS,
    sugarSpiceExchange,
    tradingPrice,
    agentWelfare,
    potentialLender,
    isPotentialBorrower,
    agentWelfareChange,
    agentImmunizeAux
  ) where

import SugarScape.Model

import FRP.FrABS

import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- GENERAL FUNCTIONS, independent of monadic / non-monadic implementation
------------------------------------------------------------------------------------------------------------------------
type BestCellMeasureFunc = (SugarScapeEnvCell -> Double) 

isDiseased :: SugarScapeAgentState -> Bool
isDiseased s = not $ null (sugAgDiseases s)

metabolismAmount :: SugarScapeAgentState -> (Double, Double)
metabolismAmount s = (sugarMetab + inc, spiceMetab + inc)
    where
        sugarMetab = sugAgSugarMetab s
        spiceMetab = sugAgSpiceMetab s
        inc = if isDiseased s then diseasedMetabolismIncrease else 0 

selectBestCells :: BestCellMeasureFunc
                    -> EnvCoord
                    -> [(EnvCoord, SugarScapeEnvCell)]
                    -> [(EnvCoord, SugarScapeEnvCell)]
selectBestCells measureFunc refCoord cs = bestShortestdistanceManhattanCells
    where
        cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
        bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
        bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

        shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattan refCoord (fst c1)) (distanceManhattan refCoord (fst c2))) bestCells
        shortestdistanceManhattan = distanceManhattan refCoord (fst $ head shortestdistanceManhattanBestCells)
        bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattan refCoord) . fst) shortestdistanceManhattanBestCells

bestMeasureSugarLevel :: BestCellMeasureFunc
bestMeasureSugarLevel c = sugEnvSugarLevel c

bestMeasureSugarAndSpiceLevel :: SugarScapeAgentState -> BestCellMeasureFunc
bestMeasureSugarAndSpiceLevel s c = agentWelfareChange s (x1, x2)
    where
        x1 = sugEnvSugarLevel c
        x2 = sugEnvSpiceLevel c

bestMeasureSugarPolutionRatio :: BestCellMeasureFunc
bestMeasureSugarPolutionRatio c = sugLvl / (1 + polLvl)
    where
        sugLvl = sugEnvSugarLevel c
        polLvl = sugEnvPolutionLevel c

excessAmountToChildBearing :: SugarScapeAgentState -> Double
excessAmountToChildBearing s = currSugar - initSugar
    where
        currSugar = sugAgSugarLevel s
        initSugar = sugAgSugarInit s

satisfiesWealthForChildBearing :: SugarScapeAgentState -> Bool
satisfiesWealthForChildBearing s = excessAmount >= 0
    where
        excessAmount = excessAmountToChildBearing s

isFertile :: SugarScapeAgentState -> Bool
isFertile s = withinRange age fertilityAgeRange
    where
        age = sugAgAge s
        fertilityAgeRange = sugAgFertAgeRange s

tooOldForChildren :: SugarScapeAgentState -> Bool
tooOldForChildren s = age > fertilityAgeMax 
    where
        age = sugAgAge s
        (_, fertilityAgeMax) = sugAgFertAgeRange s

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

neighbourIds :: SugarScapeAgentOut -> [AgentId]
neighbourIds a = map (sugEnvOccId . fromJust . sugEnvOccupier . snd) occupiedCells
    where
        env = aoEnv a
        pos = aoEnvPos a
        neighbourCells = neighbours env pos
        occupiedCells = filter (isJust . sugEnvOccupier . snd) neighbourCells

neighbourIdsM :: State SugarScapeAgentOut [AgentId]
neighbourIdsM = state (\ao -> (neighbourIds ao, ao))

createNewBorn :: (AgentId, EnvCoord)
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
filterTargetCell :: (SugarScapeEnvCellOccupier -> Bool) -> (EnvCoord, SugarScapeEnvCell) -> Bool
filterTargetCell f (coord, cell) = maybe True f mayOccupier
    where
        mayOccupier = sugEnvOccupier cell

occupierCombatable :: Double
                        -> SugarScapeTribe 
                        -> SugarScapeEnvCellOccupier
                        -> Bool
occupierCombatable myWealth myTribe occupier= differentTribe && lessWealthy
    where
        otherTribe = sugEnvOccTribe occupier
        otherWealth = sugEnvOccWealth occupier
        differentTribe = otherTribe /= myTribe
        lessWealthy = otherWealth <myWealth 

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

cellPayoff :: (EnvCoord, SugarScapeEnvCell) -> ((EnvCoord, SugarScapeEnvCell), Double)
cellPayoff (c, cell) = ((c, cell), payoff)
    where
        mayOccupier = sugEnvOccupier cell
        sugarLevel = sugEnvSugarLevel cell
        payoff = maybe sugarLevel (\occupier -> sugarLevel + (min combatReward (sugEnvOccWealth occupier))) mayOccupier
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