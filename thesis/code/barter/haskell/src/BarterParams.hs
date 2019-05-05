module BarterParams
  ( BarterParams  -- note only export data-constructor, not the accessors

  , mkBarterParams
  
  , getNumGoods
  , getConsumeArray
  , getMaxTries
  , getEquiPrice
  , isCheckEfficiency
  , getProduceGoodPriceFactor
  , isEquiInitialPrices
  , getAgentsPerGood
  , isVarySupply
  , getPriceUnitGood
  , getStdDevGood
  , getReproducePeriod
  , getTotalAgents
  , getReplacementRate
  , getMutationDelta
  , getMutationRate
  ) where

data BarterParams = BarterParams
  { agentsPerGood          :: Int
  , reproducePeriod        :: Int
  , replacementRate        :: Double
  , mutationRate           :: Double
  , mutationDelta          :: Double
  , consume                :: [Double]
  , maxTries               :: Int
  , producerShiftRate      :: Double
  , varySupply             :: Bool
  , checkEfficiency        :: Bool
  , equiInitialPrices      :: Bool

  , stdDevGood             :: Int
  , produceGoodPriceFactor :: Double
  , updateFrequency        :: Int
  , skipChartFrames        :: Int

  , shouldQuit             :: Bool
  , seed                   :: Int
  , equiPrice              :: [Double]
  } deriving Show

mkBarterParams :: BarterParams
mkBarterParams = BarterParams 
    { agentsPerGood          = 100
    , reproducePeriod        = 10
    , replacementRate        = 0.05
    , mutationRate           = 0.1
    , mutationDelta          = 0.95
    , consume                = consumeVector
    , maxTries               = 5
    , producerShiftRate      = 0.01
    , varySupply             = False
    , checkEfficiency        = False
    , equiInitialPrices      = False
    , stdDevGood             = 0
    , produceGoodPriceFactor = 0.8
    , updateFrequency        = 2
    , skipChartFrames        = 100
    , shouldQuit             = False
    , seed                   = 0
    , equiPrice              = eqPrice
    }
  where
    consumeVector = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
    eqPrice       = calcEquiPrice consumeVector

calcEquiPrice :: [Double] -> [Double]
calcEquiPrice consumeVector 
    = map (cpug /) consumeVector
  where
    pug  = length consumeVector - 1
    cpug = consumeVector !! pug

getNumGoods :: BarterParams -> Int
getNumGoods = length . consume

getConsumeArray :: BarterParams -> [Double]
getConsumeArray = consume

getMaxTries :: BarterParams -> Int
getMaxTries = maxTries

getEquiPrice :: BarterParams -> [Double]
getEquiPrice = equiPrice

isCheckEfficiency :: BarterParams -> Bool
isCheckEfficiency = checkEfficiency

getProduceGoodPriceFactor :: BarterParams -> Double
getProduceGoodPriceFactor = produceGoodPriceFactor

isEquiInitialPrices :: BarterParams -> Bool
isEquiInitialPrices = equiInitialPrices

getAgentsPerGood :: BarterParams -> Int
getAgentsPerGood = agentsPerGood

isVarySupply :: BarterParams -> Bool
isVarySupply = varySupply

getPriceUnitGood :: BarterParams -> Int
getPriceUnitGood = (+(-1))  . length . consume

getStdDevGood :: BarterParams -> Int
getStdDevGood = stdDevGood

getReproducePeriod :: BarterParams -> Int
getReproducePeriod = reproducePeriod

-- it should be possible to write it point-free with arrows?
getTotalAgents :: BarterParams -> Int
getTotalAgents params = agentsPerGood params * getNumGoods params 

getReplacementRate :: BarterParams -> Double
getReplacementRate = replacementRate

getMutationDelta :: BarterParams -> Double
getMutationDelta = mutationDelta

getMutationRate :: BarterParams -> Double
getMutationRate = mutationRate