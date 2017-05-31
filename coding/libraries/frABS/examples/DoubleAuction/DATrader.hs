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
bidOfferings :: DAAgentState -> Rand StdGen DoubleAuctionMsg 
bidOfferings s = 
	do
		acBid <- assetCashBid s
		lcBid <- loanCashBid s 
		alBid <- assetLoanBid s 
		ccBid <- collatCashBid s 

		return $ BidOffering {
	    	offeringAssetCash = acBid,
	    	offeringLoanCash = lcBid,
	    	offeringAssetLoan = alBid,
	    	offeringCollatCash = ccBid
		}

askOfferings :: DAAgentState -> Rand StdGen DoubleAuctionMsg 
askOfferings s = 
	do
		acAsk <- assetCashAsk s
		lcAsk <- loanCashAsk s 
		alAsk <- assetLoanAsk s 
		ccAsk <- collatCashAsk s 

		return $ AskOffering {
	    	offeringAssetCash = acAsk,
	    	offeringLoanCash = lcAsk,
	    	offeringAssetLoan = alAsk,
	    	offeringCollatCash = ccAsk
		}



assetCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetCashBid s = return (Just (0, 0))

loanCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
loanCashBid s = return (Just (0, 0))

assetLoanBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
assetLoanBid s = return (Just (0, 0))

collatCashBid :: DAAgentState -> Rand StdGen (Maybe OfferingData)
collatCashBid s = return (Just (0, 0))




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

		aAfterBid = sendMessage a1 (auctioneer, bos)
		aAfterAsk = sendMessage aAfterBid (auctioneer, aos)

traderAgentBehaviour :: DAAgentBehaviour
traderAgentBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< traderBehaviourFunc ain aout
------------------------------------------------------------------------------------------------------------------------