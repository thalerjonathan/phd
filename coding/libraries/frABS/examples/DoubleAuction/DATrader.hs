{-# LANGUAGE Arrows #-}
module DoubleAuction.DATrader where

-- Project-internal import first
import DoubleAuction.DAModel

import FrABS.Env.Environment
import FrABS.Agent.Agent
import FrABS.Agent.AgentUtils

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random
import Control.Monad
import qualified Data.Map as Map

-- debugging imports finally, to be easily removed in final version
import Debug.Trace

------------------------------------------------------------------------------------------------------------------------
bidOfferings :: DAAgentState -> Rand StdGen Offering 
bidOfferings s = 
	do
		acBid <- assetCashBid s
		lcBid <- loanCashBid s 
		alBid <- assetLoanBid s 
		ccBid <- collatCashBid s 

		return $ Offering {
	    	offeringAssetCash = acBid,
	    	offeringLoanCash = lcBid,
	    	offeringAssetLoan = alBid,
	    	offeringCollatCash = ccBid
		}

askOfferings :: DAAgentState -> Rand StdGen Offering 
askOfferings s = 
	do
		acAsk <- assetCashAsk s
		lcAsk <- loanCashAsk s 
		alAsk <- assetLoanAsk s 
		ccAsk <- collatCashAsk s 

		return $ Offering {
	    	offeringAssetCash = acAsk,
	    	offeringLoanCash = lcAsk,
	    	offeringAssetLoan = alAsk,
	    	offeringCollatCash = ccAsk
		}

assetCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetCashBid s = 
	do
		let cashHoldings = daTraderCash s

		let minPriceAsset = pD
		let limitPriceAsset = daTraderLimitAsset s
		-- the price for 1.0 Units of asset - will be normalized during a Match
		-- to the given amount below - the unit of this variable is CASH
		assetPriceInCash <- getRandomR (minPriceAsset, limitPriceAsset)

		-- if there is enough cash left to buy the given amount of assets
		if cashHoldings >= assetPriceInCash * tradingUnitAsset then
			-- want to BUY an asset against cash 
			-- => paying cash to seller
			-- => getting asset from seller
			-- => need to have positive amount of cash
			return (Just (assetPriceInCash, tradingUnitAsset))
			else
				-- not enough cash left to place buy-offer for asset
				return Nothing


loanCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
loanCashBid s 
	-- loan-market is open AND there is still cash left for buying a bond
	-- IMPORTANT: only generate offers if face-value V of traded bond is larger
	-- than the down-value pD as in the other case the limit-function is not monotony increasing
	| cashHoldings > tradingEpsilon && bondFaceValue > pD =
		do
			-- want to BUY a loan against cash: GIVING the seller a loan, lending money to seller
			-- => paying cash to seller (lending money to seller)
			-- => getting bond from seller (giving loan to seller)
			-- => need to have positive amount of cash

			let minPriceLoan = pD
			let limitPriceLoan = daTraderLimitLoan s
			-- the price for 1.0 Units of loans - will be normalized during a Match
			-- to the given amount below - the unit of this variable is CASH
			loanPriceInCash <- getRandomR (minPriceLoan, limitPriceLoan)

			-- calculate which amount of loans we can buy MAX
			-- upper limit: trade at most TRADING_UNIT_LOAN loans - if below take the lesser loanAmount
			-- => trading down till reaching 0
			let loanAmount = min tradingUnitLoan (cashHoldings / loanPriceInCash)

			return (Just (loanPriceInCash, loanAmount))
	-- either not enough cash left to buy a bond OR loan-market is closed
	| otherwise = return Nothing
		where
			cashHoldings = daTraderCash s

assetLoanBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetLoanBid s = -- asset-against-bond market is open, check if agent can place buy-offers
	do
		-- want to BUY an asset against loan
		-- => getting asset from seller
		-- => paying with a loan: taking loan from seller
		-- => because of taking loan: need to have enough amount of uncollateralized assets

		-- the price for 1.0 unit of assets in loans => the unit of this variable is LOANS
		let minAssetPriceInLoan = 1.0
		let limitAssetPriceInLoan = daTraderLimitAssetLoan s
		assetPriceInLoans <- getRandomR (minAssetPriceInLoan, limitAssetPriceInLoan);
		-- calculate how much loans will be taken (because selling a loan)
		let loanTakenAmount = tradingUnitAsset * assetPriceInLoans;

		-- calculate the amoung of uncollateralized assets AFTER trade. MUST ALWAYS be >= 0
		let uncollAssetsAfterTrade = daTraderAssets s
		-- collateral will be bound (taking loan from seller), thus increasing collateral obligations
		-- thus reducing uncollateralized assets after trade
		let uncollAssetsAfterTrade' = uncollAssetsAfterTrade - collateralObligationsAfterTrade s loanTakenAmount;
		-- getting asset from seller, thus increasing uncollateralized assets after trade
		let uncollAssetsAfterTrade'' = uncollAssetsAfterTrade' + tradingUnitAsset;

		-- cannot go short on assets: uncollateralized assets MUST NEVER be negative
		if uncollAssetsAfterTrade'' >= 0 then
			return (Just (assetPriceInLoans, tradingUnitAsset))
			else
				return Nothing
		
{- calculates the collateral-obligations 
	if > 0 then more loans are taken than loans are given => have debt obligations through securitization
	collateralAdjustment allows to increase/decrease the obligations e.g. when necessary to caluclate
	the obligations AFTER a trade (to calculate future obligations)
-}
collateralObligationsAfterTrade :: DAAgentState -> Double -> Double
collateralObligationsAfterTrade s collateralTraded = max 0.0 (collateral + collateralTraded)
	where
		-- when no BP Mechanism, loans given to other agents cannot be traded
		-- thus the collateral is the loans taken from other agents which implies
		-- assets as collateral 
		loansTaken = daTraderLoansTaken s
		-- loanGiven decreases the collateral in case of BP because it is available for trades
		loansGiven = daTraderLoansGiven s
		collateral = loansTaken - loansGiven

collatCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
collatCashBid s  	
	-- collateral-cash market is open, can only place an offer if there is enough cash left
	| cashHoldings > tradingEpsilon  =
		do
			let minCollateralPriceInCash = 0.0 -- minAssetPriceInCash - minLoanPriceInCash 
			let limitPriceCollateral = daTraderLimitCollateral s
			-- pick random asset price from range: is the price of 1.0 unit of assets
			assetPriceInCash <- getRandomR (minCollateralPriceInCash, limitPriceCollateral);
			-- calculate how much assets could be bought with the cash owned
			-- trade in chunks of TRADING_UNIT_ASSET but if TRADING_UNIT_ASSET > than the
			-- tradeable amount assetAmount then trade the left amount of assetAmount assets
			let assetAmount = min (cashHoldings / assetPriceInCash) tradingUnitAsset
			
			return (Just (assetPriceInCash, assetAmount))

	-- either not enough cash left to buy a bond OR loan-market is closed
	| otherwise = return Nothing
		where
			cashHoldings = daTraderCash s


assetCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetCashAsk s = return (Just (0, 0))

loanCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
loanCashAsk s = return (Just (0, 0))

assetLoanAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetLoanAsk s = return (Just (0, 0))

collatCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
collatCashAsk s = return (Just (0, 0))


traderBehaviourFunc :: DAAgentIn -> DAAgentOut -> DAAgentOut 
traderBehaviourFunc ain aout = aAfterAsk
	where
		s = aoState aout

		(bos, a0) = runAgentRandom aout (bidOfferings s)
		(aos, a1) = runAgentRandom a0 (askOfferings s)

		aAfterBid = sendMessage a1 (auctioneer, BidOffering bos)
		aAfterAsk = sendMessage aAfterBid (auctioneer, AskOffering aos)

traderAgentBehaviour :: DAAgentBehaviour
traderAgentBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< traderBehaviourFunc ain aout
------------------------------------------------------------------------------------------------------------------------