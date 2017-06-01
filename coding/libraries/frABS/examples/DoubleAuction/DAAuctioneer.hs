{-# LANGUAGE Arrows #-}
module DoubleAuction.DAAuctioneer where

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
offeringsMatch :: Maybe OfferingData -> Maybe OfferingData -> Bool
offeringsMatch mayBid mayAsk
	| isNothing mayAsk = False
	| isNothing mayBid = False
	| otherwise = (bidPrice >= askPrice)
	where
		(askPrice, _) = fromJust mayAsk
		(bidPrice, _) = fromJust mayBid

crossOver :: (AgentId, Offering) -> (AgentId, Offering) -> Maybe Match
crossOver (bidder, bids) (asker, asks) 
	-- prevent matching of bidder / asker to itself
	| bidder == asker = Nothing
	-- TODO: randomize order of market-matching
	| otherwise = if assetCashMatch 
					then Just $ createMatch AssetCash (bidder, fromJust $ (offeringAssetCash bids)) (asker, fromJust $ (offeringAssetCash asks))
					else if loanCashMatch 
						then Just $ createMatch LoanCash (bidder, fromJust $ (offeringLoanCash bids)) (asker, fromJust $ (offeringLoanCash asks))
						else if assetLoanMatch 
							then Just $ createMatch AssetLoan (bidder, fromJust $ (offeringAssetLoan bids)) (asker, fromJust $ (offeringAssetLoan asks))
							else if collatCashMatch 
								then Just $ createMatch CollateralCash (bidder, fromJust $ (offeringCollatCash bids)) (asker, fromJust $ (offeringCollatCash asks))
								else
								Nothing
		where
			assetCashMatch = offeringsMatch (offeringAssetCash bids) (offeringAssetCash asks)
			loanCashMatch = offeringsMatch (offeringLoanCash bids) (offeringLoanCash asks)
			assetLoanMatch = offeringsMatch (offeringAssetLoan bids) (offeringAssetLoan asks)
			collatCashMatch = offeringsMatch (offeringCollatCash bids) (offeringCollatCash asks)

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
findMatches :: ([(AgentId, Offering)],[(AgentId, Offering)]) -> [Maybe Match]
-- matching all biddings to all askings (note: using lazy-evaluation will not match all of them)
findMatches (bids, asks) = bidsAgainstAsks
	where
		-- TODO: randomize order of matching
		-- TODO: randomize matching of bids againgst asks OR asks against bids
		bidsAgainstAsks = [ crossOver b a | b <- bids, a <- asks ]
		asksAgainstBids = [ crossOver b a | a <- asks, b <- bids ]

accumulateOfferings :: DAAgentIn -> ([(AgentId, Offering)],[(AgentId, Offering)])
accumulateOfferings ain = onMessage ain offeringMsgAccumulate ([],[])
	where
		offeringMsgAccumulate :: ([(AgentId, Offering)],[(AgentId, Offering)]) 
									-> AgentMessage DoubleAuctionMsg 
									-> ([(AgentId, Offering)],[(AgentId, Offering)])
		offeringMsgAccumulate (bidsAcc, askAcc) (senderId, (BidOffering bos)) = ((senderId, bos) : bidsAcc, askAcc)
		offeringMsgAccumulate (bidsAcc, askAcc) (senderId, (AskOffering aos)) = (bidsAcc, (senderId, aos) : askAcc)
		offeringMsgAccumulate os _ = os

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
					  matchMarket = market } a = aAfterSeller
	where
		o = (price, amount)
		aAfterBuyer = sendMessage a (seller, SellTx market o)
		aAfterSeller = sendMessage aAfterBuyer (buyer, BuyTx market o)

auctioneerBehaviourFunc :: DAAgentIn -> DAAgentOut -> DAAgentOut 
auctioneerBehaviourFunc ain aout = maybe aout (\firstMatch -> notifyTraders (fromJust $ firstMatch) aout) mayFirstMatch
	where
		offerings = accumulateOfferings ain
		matches = findMatches offerings
		mayFirstMatch = Data.List.find isJust matches

auctioneerBehaviour :: DAAgentBehaviour
auctioneerBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< auctioneerBehaviourFunc ain aout
------------------------------------------------------------------------------------------------------------------------