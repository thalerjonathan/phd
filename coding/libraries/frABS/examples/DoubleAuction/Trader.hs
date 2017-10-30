module DoubleAuction.Trader (
	traderAgentBehaviour
  ) where

import DoubleAuction.Model

import FRP.FrABS

import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

-- TODO seems to have bug: negative cash seems to be possible (negative assets is possible if backed up by bonds)

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-STATE functions
------------------------------------------------------------------------------------------------------------------------
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

currentObligations :: DAAgentState -> Double
currentObligations s = collateralObligationsAfterTrade s 0.0

loans :: DAAgentState -> Double
loans s = loansGiven - loansTaken
	where
		loansGiven = daTraderLoansGiven s
		loansTaken = daTraderLoansTaken s

uncollateralizedAssets :: DAAgentState -> Double
uncollateralizedAssets s = daTraderAssets s - currentObligations s


-- executing a sell-transaction on this agent which means this agent is SELLING
transactSell :: Market -> OfferingData -> DAAgentState -> DAAgentState
-- SELLING an asset for cash
-- => giving asset to buyer
-- => getting cash from buyer
transactSell AssetCash (price, amount) s = s { daTraderCash = daTraderCash s + price, 
											   daTraderAssets = daTraderAssets s - amount }
-- SELLING a loan for cash
-- => collateralizing the amount of assets which correspond to the sold amount of loans
-- => getting money from buyer
transactSell LoanCash (price, amount) s = s { daTraderCash = daTraderCash s + price, 
											  daTraderLoansTaken = daTraderLoansTaken s + amount }
-- SELLING asset for loan
-- => giving asset to buyer
-- => giving loan to buyer (buyer takes a loan)
-- price is in this case the asset-price in LOANS: amount of loans for 1.0 unit of assets
-- amount is in this case the asset-amount traded
-- getNormalizedPrice returns in this case the amount of loans needed for the given asset-amount
transactSell AssetLoan (price, amount) s = s { daTraderLoansGiven = daTraderLoansGiven s + price,
											   daTraderAssets = daTraderAssets s - amount }
-- SELLING collateral for cash
-- => giving COLLATERALIZED asset to buyer (is asset + the amount of loan)
-- => getting cash from buyer
transactSell CollateralCash (price, amount) s = s { daTraderCash = daTraderCash s + price, 
													daTraderLoansGiven = daTraderLoansGiven s + amount,
													daTraderAssets = daTraderAssets s - amount }

-- executing a buy-transaction on this agent which means this agent is BUYING
transactBuy :: Market -> OfferingData -> DAAgentState -> DAAgentState
-- BUYING an asset for cash
-- => getting assets from seller
-- => paying cash to seller
transactBuy AssetCash (price, amount) s = s { daTraderCash = daTraderCash s - price, 
											  daTraderAssets = daTraderAssets s + amount }
-- BUYING a loan for cash
-- => getting loans from the seller: "un"-collateralizes assets
-- => paying cash to the seller 
transactBuy LoanCash (price, amount) s = s { daTraderCash = daTraderCash s - price, 
											 daTraderLoansGiven = daTraderLoansGiven s + amount }
-- BUYING an asset for loan
-- => getting assets from seller
-- => taking loan from seller: need to collateralize same amount of assets
-- price is in this case the asset-price in LOANS: amount of loans for 1.0 unit of assets
-- amount is in this case the asset-amount traded
-- getNormalizedPrice returns in this case the amount of loans needed for the given asset-amount
transactBuy AssetLoan (price, amount) s = s { daTraderLoansTaken = daTraderLoansTaken s + price, 
											  daTraderAssets = daTraderAssets s + amount }
-- BUYING collateral for cash
-- => getting COLLATERALIZED asset from seller (is asset + the amount of loan)
-- => giving cash to seller
transactBuy CollateralCash (price, amount) s = s { daTraderCash = daTraderCash s - price, 
												   daTraderLoansTaken = daTraderLoansTaken s + amount,
												   daTraderAssets = daTraderAssets s + amount }


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
		if cashHoldings >= (assetPriceInCash * tradingUnitAsset) then
			-- want to BUY an asset against cash 
			-- => paying cash to seller
			-- => getting asset from seller
			-- => need to have positive amount of cash
			return $ Just (assetPriceInCash, tradingUnitAsset)
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

			return $ Just (loanPriceInCash, loanAmount)
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
		assetPriceInLoans <- getRandomR (minAssetPriceInLoan, limitAssetPriceInLoan)
		-- calculate how much loans will be taken (because selling a loan)
		let loanTakenAmount = tradingUnitAsset * assetPriceInLoans

		-- calculate the amoung of uncollateralized assets AFTER trade. MUST ALWAYS be >= 0
		let uncollAssetsAfterTrade = daTraderAssets s
		-- collateral will be bound (taking loan from seller), thus increasing collateral obligations
		-- thus reducing uncollateralized assets after trade
		let uncollAssetsAfterTrade' = uncollAssetsAfterTrade - collateralObligationsAfterTrade s loanTakenAmount
		-- getting asset from seller, thus increasing uncollateralized assets after trade
		let uncollAssetsAfterTrade'' = uncollAssetsAfterTrade' + tradingUnitAsset

		-- cannot go short on assets: uncollateralized assets MUST NEVER be negative
		if uncollAssetsAfterTrade'' >= 0 then
			return (Just (assetPriceInLoans, tradingUnitAsset))
			else
				return Nothing
		
collatCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
collatCashBid s  	
	-- collateral-cash market is open, can only place an offer if there is enough cash left
	| cashHoldings > tradingEpsilon  =
		do
			let minCollateralPriceInCash = 0.0 -- minAssetPriceInCash - minLoanPriceInCash 
			let limitPriceCollateral = daTraderLimitCollateral s
			-- pick random asset price from range: is the price of 1.0 unit of assets
			assetPriceInCash <- getRandomR (minCollateralPriceInCash, limitPriceCollateral)
			-- calculate how much assets could be bought with the cash owned
			-- trade in chunks of TRADING_UNIT_ASSET but if TRADING_UNIT_ASSET > than the
			-- tradeable amount assetAmount then trade the left amount of assetAmount assets
			let assetAmount = min (cashHoldings / assetPriceInCash) tradingUnitAsset
			
			return $ Just (assetPriceInCash, assetAmount)

	-- either not enough cash left to buy a bond OR loan-market is closed
	| otherwise = return Nothing
		where
			cashHoldings = daTraderCash s

assetCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetCashAsk s 
	-- if there are still uncollateralized assets left, create a sell-offer
	| uncollAssets > tradingUnitAsset =
		do
			-- want to SELL an asset against cash 
			-- => giving asset to buayer
			-- => getting cash from buyer
			-- => can only do so it if there are uncollateralized assets left

			-- this is always the price for 1.0 Units of asset - will be normalized during a Match
			-- to the given amount below - the unit of this variable is CASH

			let limitPriceAsset = daTraderLimitAsset s
			let maxAssetPriceInCash = pU
			assetPriceInCash <- getRandomR (limitPriceAsset, maxAssetPriceInCash)
			return $ Just (assetPriceInCash, tradingUnitAsset)
	-- no more (not enough) uncollateralized assets left, can't sell anymore, don't place a sell-offer
	| otherwise = return Nothing
	where
		uncollAssets = uncollateralizedAssets s

loanCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
loanCashAsk s 
	-- loan-market is open AND there are still uncollateralized assets left
	-- agent can place a sell-offer because when selling a loan, it needs to be secured by collateralizing 
	-- the same amount of assets
	-- IMPORTANT: only generate offers if face-value V of traded bond is larger
	-- than the down-value pD as in the other case the limit-function is not monotony increasing
	| tradeableLoans > tradingEpsilon && bondFaceValue > pD =
		do
			-- want to SELL a loan against cash: borrowing money from buyer by TAKING a loan
			-- => collateralize assets of the same amount as security (taking loan)
			-- => getting money from buyer  (borrowing money from buyer) 
			-- => need to have enough uncollateralized assets

			-- this is always the price for 1.0 Units of loans - will be normalized during a Match
			-- to the given amount below - the unit of this variable is CASH
			let limitPriceLoan = daTraderLimitLoan s
			let maxLoanPriceInCash = bondFaceValue
			loanPriceInCash <- getRandomR (limitPriceLoan, maxLoanPriceInCash)
			
			-- the maximum of loans we can sell is the uncollateralized assets left (1:1 relationship when collateralizing)
			-- but don't trad everything at once, break down into small chungs: Markets.TRADING_UNIT_LOAN
			let loanAmount = min tradeableLoans tradingUnitLoan

			return $ Just (loanPriceInCash, loanAmount)

	-- either no more uncollaterlized assets left, can't sell loans because don't have assets to
	-- secure the loan by collateralizing OR loan-market is closed
	| otherwise = return Nothing
	where
		tradeableLoans = loans s

-- asset-against-bond market is open, check if this agent can place sell-offers
assetLoanAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetLoanAsk s =
	do
		-- want to SELL a loan against an asset 
		-- => giving asset to buyer
		-- => getting bond from buyer: giving loan
		-- => because of giving asset: need to have enough amount of uncollateralized assets
		
		-- the price for 1.0 unit of assets in loans => the unit of this variable is LOANS
		let limitPriceAssetLoans = daTraderLimitAssetLoan s
		let maxAssetPriceInLoans = pU / bondFaceValue
		assetPriceInLoans <- getRandomR (limitPriceAssetLoans, maxAssetPriceInLoans)
		-- calculating the amount loans which will be given to buyer
		let loanGivingAmount = tradingUnitAsset * assetPriceInLoans
		
		-- calculate the amoung of uncollateralized assets AFTER trade. MUST ALWAYS be >= 0
		let uncollAssetsAfterTrade0 = daTraderAssets s
		-- collateral will be freed (giving loan), thus reducing collateral obligations
		-- thus increasing uncollateralized assets after trade
		let uncollAssetsAfterTrade1 = uncollAssetsAfterTrade0 - collateralObligationsAfterTrade s (-loanGivingAmount)
		-- giving asset to buyer, thus decreasing uncollateralized assets after trade
		let uncollAssetsAfterTrade2 = uncollAssetsAfterTrade1 - tradingUnitAsset

		if uncollAssetsAfterTrade2 >= 0 then
			return (Just (assetPriceInLoans, tradingUnitAsset))
			else
				return Nothing

collatCashAsk :: DAAgentState -> Rand StdGen (Maybe OfferingData)
collatCashAsk s 
	-- collateral-cash market is open, can only place offer if there are any collateralized assets around
	| currOb > tradingEpsilon =
		do
			-- pick a random price from range: is the price for 1.0 Unit of (collateral) assets
			let limitPriceCollateral = daTraderLimitCollateral s
			let maxCollateralPriceInCash = pU - bondFaceValue
			assetPriceInCash <- getRandomR (limitPriceCollateral, maxCollateralPriceInCash)
			-- calculate the amount of assets to trade: don't trade all but in small chunks of TRADING_UNIT_ASSET
			-- if TRADING_UNIT_ASSET > than the tradeable amount of currentObligations take the rest of 
			-- currentObligations to trade down to 0
			let assetAmount = min currOb tradingUnitAsset

			return $ Just (assetPriceInCash, assetAmount)
	| otherwise = return Nothing 
	where
		-- how many assets are bound through bonds
		currOb = currentObligations s
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
--  AGENT-BEHAVIOUR MONADIC implementation
------------------------------------------------------------------------------------------------------------------------
traderBehaviourFuncM :: Double -> DAAgentIn -> State DAAgentOut ()
traderBehaviourFuncM _ ain =
	do
		sendOfferingsM
		receiveTransactionsM ain

sendOfferingsM :: State DAAgentOut ()
sendOfferingsM =
	do
		s <- agentStateM
		bos <- agentRandomM (bidOfferings s)
		aos <- agentRandomM (askOfferings s)

		sendMessageM (auctioneer, BidOffering bos)
		sendMessageM (auctioneer, AskOffering aos)

receiveTransactionsM :: DAAgentIn -> State DAAgentOut ()
receiveTransactionsM ain = onMessageMState handleTxMsgM ain
	where
		handleTxMsgM :: AgentMessage DoubleAuctionMsg -> State DAAgentOut ()
		handleTxMsgM (_, (SellTx m o)) = updateAgentStateM (transactSell m o)
		handleTxMsgM (_, (BuyTx m o)) = updateAgentStateM (transactBuy m o)
		handleTxMsgM _ = return ()
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- AGENT-BEHAVIOUR NON-monadic implementation
------------------------------------------------------------------------------------------------------------------------
traderBehaviourFunc :: Double -> DAAgentIn -> DAAgentOut -> DAAgentOut
traderBehaviourFunc _ ain ao = sendOfferings $ receiveTransactions ain ao

sendOfferings :: DAAgentOut -> DAAgentOut
sendOfferings ao = aAfterAsk
	where
		s = agentState ao

		(bos, ao0) = agentRandom (bidOfferings s) ao 
		(aos, ao1) = agentRandom (askOfferings s) ao0

		aAfterBid = sendMessage (auctioneer, BidOffering bos) ao1
		aAfterAsk = sendMessage (auctioneer, AskOffering aos) aAfterBid

receiveTransactions :: DAAgentIn -> DAAgentOut -> DAAgentOut
receiveTransactions ain ao = onMessage handleTxMsg ain ao
	where
		handleTxMsg :: AgentMessage DoubleAuctionMsg -> DAAgentOut -> DAAgentOut
		handleTxMsg (_, (SellTx m o)) ao = updateAgentState (transactSell m o) ao
		handleTxMsg (_, (BuyTx m o)) ao = updateAgentState (transactBuy m o) ao
		handleTxMsg _ ao = ao
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
traderAgentBehaviour :: DAAgentBehaviour
traderAgentBehaviour = agentMonadicIgnoreEnv traderBehaviourFuncM -- agentPureIgnoreEnv traderBehaviourFunc
------------------------------------------------------------------------------------------------------------------------