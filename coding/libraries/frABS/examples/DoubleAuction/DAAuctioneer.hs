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
auctioneerBehaviourFunc :: DAAgentIn -> DAAgentOut -> DAAgentOut 
auctioneerBehaviourFunc ain aout = aout --trace ("bidOfferings: " ++ (show bidOfferings) ++ "\naskOfferings: " ++ (show askOfferings)) aout
	where
		(bidOfferings, askOfferings) = onMessage ain offeringMsgAccumulate ([],[])

offeringMsgAccumulate :: ([(AgentId, Offering)],[(AgentId, Offering)]) 
							-> AgentMessage DoubleAuctionMsg 
							-> ([(AgentId, Offering)],[(AgentId, Offering)])
offeringMsgAccumulate (bidsAcc, askAcc) (senderId, (BidOffering bos)) = ((senderId, bos) : bidsAcc, askAcc)
offeringMsgAccumulate (bidsAcc, askAcc) (senderId, (AskOffering aos)) = (bidsAcc, (senderId, aos) : askAcc)
offeringMsgAccumulate os _ = os


-- TODO: we have many ways to do the transaction
-- 		1. simply send a message to both traders => traders have to check for incoming messages first and transact before sending new offerings to prevent violation of budget constraints
-- 		2. start a transact-conversation with both traders
-- 		3. send a message to one of the traders with the id of the other trader so that this trader then initiates a transaction

auctioneerBehaviour :: DAAgentBehaviour
auctioneerBehaviour = proc ain ->
    do
        let aout = agentOutFromIn ain
        returnA -< auctioneerBehaviourFunc ain aout
------------------------------------------------------------------------------------------------------------------------