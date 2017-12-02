module DoubleAuction.Model (
    Market (..),
    Match (..),
    OfferingData,
    Offering (..),
    DoubleAuctionMsg (..),
    DAAgentState (..),

    DAEnvironment,

    DAAgentDef,
    DAAgentBehaviour,
    DAAgentIn,
    DAAgentOut,
    DAAgentObservable,

    pU,
    pD,
    bondFaceValue,
    limitPriceAsset,
    limitPriceLoan,
    cashEndow,
    assetEndow,
    tradingUnitAsset,
    tradingUnitLoan,
    tradingEpsilon,
    auctioneer,
    isAuctioneer,
    isTrader,
    shuffleRandom,
  ) where

import FRP.FrABS

import FRP.Yampa

import Control.Monad.Random
import Data.Array.ST
import GHC.Arr

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data Market = AssetCash | LoanCash | AssetLoan | CollateralCash deriving (Eq, Show)

data Match = Match {
	matchMarket :: Market,

	matchPrice :: Double,
	matchNormPrice :: Double,
	matchAmount :: Double,

	matchBidOffer :: OfferingData,
	matchAskOffer :: OfferingData,

	matchBuyer :: AgentId,
	matchSeller :: AgentId
}

type OfferingData = (Double, Double)	-- fst: price, snd: amount
data Offering = Offering {
		offeringAssetCash :: Maybe OfferingData,
    	offeringLoanCash :: Maybe OfferingData,
    	offeringAssetLoan :: Maybe OfferingData,
    	offeringCollatCash :: Maybe OfferingData
	} deriving (Eq, Show)

data DoubleAuctionMsg =
    BidOffering Offering
  | AskOffering Offering
  | SellTx Market OfferingData
  | BuyTx Market OfferingData
    deriving (Eq, Show)

data DAAgentState = 
	TraderState {
    	daTraderOptimism :: Double,				-- optimism factor, the probability the agent assigns to the event of tomorrow UP
    	daTraderLimitAsset :: Double,			-- expected Value of the Asset in CASH
    	daTraderLimitLoan :: Double,			-- expected Value of the loan in CASH
    	daTraderLimitAssetLoan :: Double,		-- expected value of asset/loan price: the value of an asset in LOANS
    	daTraderLimitCollateral :: Double,		-- expected value of collateral/cash price: the value of an asset in cash

    	daTraderCash :: Double,					-- amount of consumption-good endowment (cash) still available
    	daTraderAssets :: Double,				-- amount of assets this agent owns REALLY 
    	daTraderLoansTaken :: Double,			-- the amount of loans sold to other agents for cash or assets. is the amount of collateralized assets
    	daTraderLoansGiven :: Double 			-- the amount of loans bought from other agents for cash or assets. is the amount of UN-collateralized assets
	} 
  | AuctioneerState     -- NOTE: the auctioneer has no domain-specific state
    deriving (Show, Read)

type DAEnvironment = Network ()

type DAAgentDef = AgentDef DAAgentState DoubleAuctionMsg DAEnvironment
type DAAgentBehaviour = AgentBehaviour DAAgentState DoubleAuctionMsg DAEnvironment
type DAAgentIn = AgentIn DAAgentState DoubleAuctionMsg DAEnvironment
type DAAgentOut = AgentOut DAAgentState DoubleAuctionMsg DAEnvironment
type DAAgentObservable = AgentObservable DAAgentState
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- MODEL-PARAMETERS
------------------------------------------------------------------------------------------------------------------------
pU :: Double
pU = 1.0

pD :: Double
pD = 0.2

bondFaceValue :: Double
bondFaceValue = 0.5

limitPriceAsset :: Double -> Double
limitPriceAsset h = h * pU + (1.0 - h) * pD

limitPriceLoan :: Double -> Double
limitPriceLoan h = h * bondFaceValue + (1.0 - h) * pD

cashEndow :: Double 
cashEndow = 1.0

assetEndow :: Double
assetEndow = 1.0

tradingUnitAsset :: Double
tradingUnitAsset = 0.1

tradingUnitLoan :: Double
tradingUnitLoan = 0.2

tradingEpsilon :: Double
tradingEpsilon = 0.000000001

auctioneer :: AgentId
auctioneer = 0
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- BOILERPLATE STUFF
------------------------------------------------------------------------------------------------------------------------
isAuctioneer :: DAAgentState -> Bool
isAuctioneer (AuctioneerState {}) = True
isAuctioneer _ = False

isTrader :: DAAgentState -> Bool
isTrader (TraderState {}) = True
isTrader _ = False

-- taken from https://wiki.haskell.org/Random_shuffle
shuffleRandom :: RandomGen g => [a] -> Rand g [a]
shuffleRandom xs = 
  do
    let l = length xs
    rands <- forM [0..(l-2)] $ \i -> getRandomR (i, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

------------------------------------------------------------------------------------------------------------------------