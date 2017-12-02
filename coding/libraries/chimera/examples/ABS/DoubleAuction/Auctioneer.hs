module DoubleAuction.Auctioneer (
	auctioneerBehaviour
  ) where

import DoubleAuction.Model

import FRP.FrABS

import Data.Maybe
import Data.List
import System.Random
import Control.Monad.Random

------------------------------------------------------------------------------------------------------------------------
offeringsMatch :: Maybe OfferingData -> Maybe OfferingData -> Maybe (OfferingData, OfferingData)
offeringsMatch mayBid mayAsk =
	do
		bid@(bidPrice, _) <- mayBid
		ask@(askPrice, _) <- mayAsk
		if (bidPrice >= askPrice) then
			return (bid, ask)
			else
				Nothing

crossOver :: ((AgentId, Offering), (AgentId, Offering)) -> Rand StdGen (Maybe Match)
crossOver (b@(bidder, bids), a@(asker, asks))
	-- prevent matching of bidder / asker to itself
	| bidder == asker = return Nothing
	| otherwise = 
		do
			randMarketPerm <- marketPermuation [AssetCash, LoanCash, AssetLoan, CollateralCash]
			return $ matchMarketPerms randMarketPerm b a

		where
			matchMarketPerms :: [Market] -> (AgentId, Offering) -> (AgentId, Offering) -> Maybe Match
			matchMarketPerms [] _ _ = Nothing
			matchMarketPerms (m:ms) b@(bidder, bids) a@(asker, asks) =
				maybe (matchMarketPerms ms b a) 
					  (\(bidOffering, askOffering) -> 
						Just $ createMatch m (bidder, bidOffering) (asker, askOffering)) 
					  (matchMarket m bids asks)

			matchMarket :: Market -> Offering -> Offering -> Maybe (OfferingData, OfferingData)
			matchMarket AssetCash bids asks = offeringsMatch (offeringAssetCash bids) (offeringAssetCash asks)
			matchMarket LoanCash bids asks = offeringsMatch (offeringLoanCash bids) (offeringLoanCash asks)
			matchMarket AssetLoan bids asks = offeringsMatch (offeringAssetLoan bids) (offeringAssetLoan asks)
			matchMarket CollateralCash bids asks = offeringsMatch (offeringCollatCash bids) (offeringCollatCash asks)

			marketPermuation :: [Market] -> Rand StdGen [Market]
			marketPermuation markets = 
				do
					let n = length markets
					let permCount = foldr (*) 1 [1..n]
					randPermIdx <- getRandomR (0, permCount-1)
					return $ (permutations markets) !! randPermIdx

			createMatch :: Market -> (AgentId, OfferingData) -> (AgentId, OfferingData) -> Match
			createMatch m (bidder, bid@(bidPrice, bidAmount)) (asker, ask@(askPrice, askAmount)) = 
				Match {
					matchMarket = m,

					matchPrice = price,
					matchAmount = amount,
					matchNormPrice = normPrice,

					matchBidOffer = bid,
					matchAskOffer = ask,
					
					matchBuyer = bidder,
					matchSeller = asker
				}
				where
					price = (bidPrice + askPrice) * 0.5
					amount = min bidAmount askAmount
					normPrice = price * amount

-- ignoring network-structure for now and assuming complete graph
findMatches :: ([(AgentId, Offering)],[(AgentId, Offering)]) -> Rand StdGen [Maybe Match]
-- matching all biddings to all askings (note: using lazy-evaluation will not match all of them)
findMatches (bids, asks) = 
	do
		bidsShuffled <- shuffleRandom bids
		asksShuffled <- shuffleRandom asks
		
		let bidsAgainstAsks = mapM crossOver [ (b, a) | b <- bids, a <- asks ]
		let asksAgainstBids = mapM crossOver [ (b, a) | a <- asks, b <- bids ]

		bidsFirst <- getRandom

		if bidsFirst then
			bidsAgainstAsks
			else
				asksAgainstBids

accumulateOfferings :: DAAgentIn -> ([(AgentId, Offering)],[(AgentId, Offering)])
accumulateOfferings ain = onMessage offeringMsgAccumulate ain ([],[])
	where
		offeringMsgAccumulate :: AgentMessage DoubleAuctionMsg 
									-> ([(AgentId, Offering)],[(AgentId, Offering)])
									-> ([(AgentId, Offering)],[(AgentId, Offering)]) 
		offeringMsgAccumulate (senderId, (BidOffering bos)) (bidsAcc, askAcc) = ((senderId, bos) : bidsAcc, askAcc)
		offeringMsgAccumulate (senderId, (AskOffering aos)) (bidsAcc, askAcc) = (bidsAcc, (senderId, aos) : askAcc)
		offeringMsgAccumulate _ os = os

-- NOTE we have many ways to do the transaction
-- 		1. simply send a message to both traders => traders have to check for incoming messages first and transact before sending new offerings to prevent violation of budget constraints
--		next one only possible in sequential-strategy
-- 		2. start a transact-conversation with both traders
-- 		3. send a message to one of the traders with the id of the other trader so that this trader then initiates a transaction
notifyTraders :: Match -> DAAgentOut -> DAAgentOut
notifyTraders Match { matchSeller = seller,
					  matchBuyer = buyer,
					  matchAmount = amount,
					  matchNormPrice = price,
					  matchMarket = market } a = aAfterBuyer
	where
		o = (price, amount)
		aAfterSeller = sendMessage (seller, SellTx market o) a 
		aAfterBuyer = sendMessage (buyer, BuyTx market o) aAfterSeller

-- NOTE: for the auctioneer we don't provide any monadic implementation using the state-monad because the auctioneers behaviour is domain-stateless 
auctioneerBehaviourFunc :: Double -> DAAgentIn -> DAAgentOut -> DAAgentOut 
auctioneerBehaviourFunc _ ain a = maybe a' (\firstMatch -> notifyTraders (fromJust $ firstMatch) a') mayFirstMatch
	where
		offerings = accumulateOfferings ain
		(matches, a') = agentRandom (findMatches offerings) a 
		mayFirstMatch = Data.List.find isJust matches

auctioneerBehaviour :: DAAgentBehaviour
auctioneerBehaviour = agentPureIgnoreEnv auctioneerBehaviourFunc
------------------------------------------------------------------------------------------------------------------------