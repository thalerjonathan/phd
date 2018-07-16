module Common 
  (
    agentOut
  , agentOutObservable
  , isDead
  , kill
  , newAgent
  , isObservable

  , sugObservableFromState
    
  , isDiseased
  , metabolismAmount

  , BestCellMeasureFunc
  , selectBestCells
  , bestCellFunc
  , bestMeasureSugarLevel
  , bestMeasureSpiceLevel
  , bestMeasureSugarAndSpiceLevel
  , bestMeasureSugarPolutionRatio
  , bestMeasureSugarAndSpicePolutionRatio

  , unoccupiedNeighbourhoodOfNeighbours

  , withinRange
  , neighbourIds
  , createNewBorn

  , poluteCell

  , randomAgent

  , cellOccupier
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Random
import           Data.List.Split
import qualified FRP.BearRiver as BR

import           Data.Maybe
import           Data.List

import           Discrete
import           Model

------------------------------------------------------------------------------------------------------------------------
-- GENERAL FUNCTIONS, independent of monadic / non-monadic implementation
------------------------------------------------------------------------------------------------------------------------
agentOut :: SugAgentOut g
agentOut = agentOutAux Nothing

agentOutObservable :: SugAgentObservable 
                   -> SugAgentOut g
agentOutObservable o = agentOutAux $ Just o

agentOutAux :: Maybe SugAgentObservable
            -> SugAgentOut g
agentOutAux mo = SugAgentOut 
  { sugAoKill       = BR.NoEvent
  , sugAoNew        = []
  , sugAoObservable = mo
  }

isDead :: SugAgentOut g -> Bool
isDead ao = BR.isEvent $ sugAoKill ao

kill :: SugAgentOut g -> SugAgentOut g
kill ao = ao { sugAoKill = BR.Event () }

newAgent :: AgentId
         -> SugAgent g
         -> SugAgentOut g
         -> SugAgentOut g
newAgent aid a ao 
  = ao { sugAoNew = (aid, a) : sugAoNew ao }

isObservable :: SugAgentOut g -> Bool
isObservable ao = isJust $ sugAoObservable ao

sugObservableFromState :: SugAgentState -> SugAgentObservable
sugObservableFromState s = SugAgentObservable
  { sugObsCoord    = sugAgCoord s 
  , sugObsGender   = sugAgGender s
  , sugObsVision   = sugAgVision s
  , sugObsDiseases = sugAgDiseases s
  , sugObsTribe    = sugAgTribe s
  }

type BestCellMeasureFunc = (SugEnvCell -> Double) 

isDiseased :: SugAgentState -> Bool
isDiseased = not . null . sugAgDiseases

metabolismAmount :: SugAgentState -> (Double, Double)
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
                -> [(Discrete2dCoord, SugEnvCell)]
                -> [(Discrete2dCoord, SugEnvCell)]
selectBestCells measureFunc refCoord cs = bestShortestdistanceManhattanCells
  where
    cellsSortedByMeasure = sortBy (\c1 c2 -> compare (measureFunc $ snd c2) (measureFunc $ snd c1)) cs
    bestCellMeasure = measureFunc $ snd $ head cellsSortedByMeasure
    bestCells = filter ((==bestCellMeasure) . measureFunc . snd) cellsSortedByMeasure

    shortestdistanceManhattanBestCells = sortBy (\c1 c2 -> compare (distanceManhattanDisc2d refCoord (fst c1)) (distanceManhattanDisc2d refCoord (fst c2))) bestCells
    shortestdistanceManhattan = distanceManhattanDisc2d refCoord (fst $ head shortestdistanceManhattanBestCells)
    bestShortestdistanceManhattanCells = filter ((==shortestdistanceManhattan) . (distanceManhattanDisc2d refCoord) . fst) shortestdistanceManhattanBestCells

unoccupiedNeighbourhoodOfNeighbours :: Discrete2dCoord 
                                    -> SugEnvironment 
                                    -> STM [(Discrete2dCoord, SugEnvCell)]
unoccupiedNeighbourhoodOfNeighbours coord e = do
  ncs <- neighbours coord False e
  -- NOTE: this calculates the cells which are in the initial neighbourhood and in the neighbourhood of all the neighbours
  nncsDupl <- foldM (\acc (coord', _) -> do
                        ncs' <- neighbours coord' False e
                        return $ ncs' ++ acc) ncs ncs
  -- NOTE: the nncs are not unique, remove duplicates
  let nncsUnique = nubBy (\(coord1, _) (coord2, _) -> (coord1 == coord2)) nncsDupl
  return $ filter (isNothing . sugEnvOccupier . snd) nncsUnique

bestMeasureSugarLevel :: BestCellMeasureFunc
bestMeasureSugarLevel = sugEnvSugarLevel

bestMeasureSpiceLevel :: BestCellMeasureFunc
bestMeasureSpiceLevel = sugEnvSpiceLevel

bestMeasureSugarAndSpiceLevel :: BestCellMeasureFunc
bestMeasureSugarAndSpiceLevel c = sugLvl + spiLvl
  where
    sugLvl = sugEnvSugarLevel c
    spiLvl = sugEnvSugarLevel c

bestMeasureSugarPolutionRatio :: BestCellMeasureFunc
bestMeasureSugarPolutionRatio c 
    = sugLvl / (1 + polLvl)
  where
    sugLvl = sugEnvSugarLevel c
    polLvl = sugEnvPolutionLevel c

bestMeasureSugarAndSpicePolutionRatio :: BestCellMeasureFunc
bestMeasureSugarAndSpicePolutionRatio c 
    = (sugLvl + spiLvl) / (1 + polLvl)
  where
    sugLvl = sugEnvSugarLevel c
    spiLvl = sugEnvSugarLevel c
    polLvl = sugEnvPolutionLevel c

withinRange :: (Ord a) => a -> (a, a) -> Bool
withinRange a (l, u) = a >= l && a <= u

neighbourIds :: SugEnvironment
             -> SugAgentState
             -> STM [AgentId]
neighbourIds e s = do
    ncs <- neighbourCells coord False e -- NOTE: this includes only neighbours, never self, never required in this function
    let occupiedCells = filter (isJust . sugEnvOccupier) ncs
    return $ map (sugEnvOccId . fromJust . sugEnvOccupier) occupiedCells
  where
    coord = sugAgCoord s  

createNewBorn :: RandomGen g
              => (AgentId, Discrete2dCoord)
              -> (Double, Double, Double, Int, SugCulturalTag, SugImmuneSystem)
              -> (Double, Double, Double, Int, SugCulturalTag, SugImmuneSystem)
              -> (AgentId -> SugAgentState -> SugAgent g)
              -> Rand g (SugAgent g, SugAgentState)
createNewBorn idCoord
                (sugEndowFather, sugarMetabFather, spiceMetabFather, visionFather, cultureFather, immuneSysBornFather)
                (sugEndowMother, sugarMetabMother, spiceMetabMother, visionMother, cultureMother, immuneSysBornMother)
                beh = do
  newBornSugarMetab <- crossover (sugarMetabFather, sugarMetabMother)
  newBornSpiceMetab <- crossover (spiceMetabFather, spiceMetabMother)
  newBornVision <- crossover (visionFather, visionMother)
  newBornCulturalTag <- crossoverBools cultureFather cultureMother
  newBornImmuneSystem <- crossoverBools immuneSysBornFather immuneSysBornMother

  let newBornSugarEndow = sugEndowFather + sugEndowMother

  randomAgent
      idCoord
      beh
      (\s -> s { sugAgSugarMetab = newBornSugarMetab
              , sugAgSpiceMetab = newBornSpiceMetab
              , sugAgVision = newBornVision
              , sugAgSugarLevel = newBornSugarEndow
              , sugAgSugarInit = newBornSugarEndow
              , sugAgCulturalTag = newBornCulturalTag
              , sugAgTribe = calculateTribe newBornCulturalTag
              , sugAgImmuneSys = newBornImmuneSystem
              , sugAgImmuneSysBorn = newBornImmuneSystem
              , sugAgDiseases = [] 
              })

poluteCell :: Double 
           -> Discrete2dCoord 
           -> SugEnvironment 
           -> STM ()
poluteCell polutionIncrease coord e 
    | _enablePolution_ = do
      c <- cellAt coord e
      let c' = c { sugEnvPolutionLevel = polutionIncrease + sugEnvPolutionLevel c }
      changeCellAt coord c' e
    | otherwise = return ()
      
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- UTILS
------------------------------------------------------------------------------------------------------------------------
cellOccupier :: AgentId -> SugAgentState -> SugEnvCellOccupier
cellOccupier aid s = SugEnvCellOccupier {
                        sugEnvOccId = aid,
                        sugEnvOccTribe = sugAgTribe s,
                        sugEnvOccWealth = wealth
                    }
  where
      wealth = (sugAgSugarLevel s) + (sugAgSpiceLevel s)

calculateTribe :: SugCulturalTag -> SugTribe
calculateTribe tag
    | falseCount >= trueCount = Blue
    | otherwise = Red
  where
    falseCount = length $ filter (==False) tag 
    trueCount = length $ filter (==True) tag 
       
-- NOTE: the tags must have same length, this could be enforced statically through types if we had a dependent type-system
cultureContact :: RandomGen g
               => SugCulturalTag 
               -> SugCulturalTag 
               -> Rand g SugCulturalTag
cultureContact tagActive tagPassive = do
    randIdx <- getRandomR (0, tagLength - 1)
    return $ flipCulturalTag tagActive tagPassive randIdx
  where
    tagLength = length tagActive
        
-- NOTE: the tags must have same length, this could be enforced statically through types if we had a dependent type-system
flipCulturalTag :: SugCulturalTag 
                -> SugCulturalTag 
                -> Int 
                -> SugCulturalTag
flipCulturalTag [] [] _ = []
flipCulturalTag tagActive tagPassive idx = map (\(i, a, p) -> if i == idx then a else p) (zip3 [0..len-1] tagActive tagPassive) 
  where
    len = length tagActive

crossoverBools :: RandomGen g
               => [Bool] 
               -> [Bool]  
               -> Rand g [Bool] 
crossoverBools ts1 ts2 = do
  randTags <- replicateM (length ts1) (getRandomR (True, False))
  return $ map (\(t1, t2, randT) -> if t1 == t2 then t1 else randT) (zip3 ts1 ts2 randTags)

crossover :: RandomGen g
          => (a, a) 
          -> Rand g a
crossover (x, y) = do
  takeX <- getRandomR (True, False)
  if takeX 
    then return x
    else return y

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

randomAgent :: (MonadRandom m, MonadSplit g m, RandomGen g)
            => (AgentId, Discrete2dCoord)
            -> (AgentId -> SugAgentState -> SugAgent g)
            -> (SugAgentState -> SugAgentState)
            -> m (SugAgent g, SugAgentState)
randomAgent (agentId, coord) beh sup = do
  -- NOTE: need to split here otherwise agents would end up with the same random-values when not already splitting in the calling function
  _rng <- getSplit

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
  
  let s = SugAgentState {
    sugAgCoord            = coord
  , sugAgSugarMetab       = randSugarMetab
  , sugAgSpiceMetab       = spiceMetab
  , sugAgVision           = randVision
  , sugAgSugarLevel       = randSugarEndowment
  , sugAgSugarInit        = randSugarEndowment
  , sugAgSpiceInit        = spiceEndow
  , sugAgSpiceLevel       = spiceEndow
  , sugAgMaxAge           = randMaxAge
  , sugAgGender           = randGender
  , sugAgFertAgeRange     = (randMinFert, randMaxFert)
  , sugAgChildren         = []
  , sugAgAge              = 0.0
  , sugAgCulturalTag      = randCulturalTag
  , sugAgTribe            = calculateTribe randCulturalTag
  , sugAgBorrowingCredits = []
  , sugAgLendingCredits   = []
  , sugAgImmuneSys        = immuneSys
  , sugAgImmuneSysBorn    = immuneSys
  , sugAgDiseases         = diseases
  }

  let s'   = sup s
  let abeh = beh agentId s'

  return (abeh, s')