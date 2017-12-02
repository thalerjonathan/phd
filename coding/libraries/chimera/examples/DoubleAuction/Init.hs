module DoubleAuction.Init (
    initDoubleAuction
  ) where

import DoubleAuction.Model 
import DoubleAuction.Auctioneer
import DoubleAuction.Trader

import FRP.FrABS

import FRP.Yampa

import System.Random
import Control.Monad.Random

initDoubleAuction :: Int -> IO ([DAAgentDef], DAEnvironment)
initDoubleAuction n = 
  do
    auctioneer <- evalRandIO (createDAAuctioneer auctioneer)
    traders <- evalRandIO $ mapM (createDATrader n) [1..n]
    let envNet = createDeterministicNetwork (Complete n) unitEdgeLabeler

    return (auctioneer : traders, envNet)

createDATrader :: Int -> AgentId -> Rand StdGen DAAgentDef
createDATrader n aid = 
    do
        rng <- getSplit

        let h = (fromIntegral aid) / (fromIntegral (n + 1))

        let s = TraderState {
          daTraderOptimism = h,
          daTraderLimitAsset = limitPriceAsset h,
          daTraderLimitLoan = limitPriceLoan h,
          daTraderLimitAssetLoan = (limitPriceAsset h) / (limitPriceLoan h),
          daTraderLimitCollateral = (limitPriceAsset h) - (limitPriceLoan h),

          daTraderCash = cashEndow,
          daTraderAssets = assetEndow,
          daTraderLoansTaken = 0.0,
          daTraderLoansGiven = 0.0
        }

        let adef = AgentDef {
           adId = aid,
           adState = s,
           adConversation = Nothing,
           adInitMessages = NoEvent,
           adBeh = traderAgentBehaviour,
           adRng = rng 
        }

        return adef

createDAAuctioneer :: AgentId -> Rand StdGen DAAgentDef
createDAAuctioneer aid = 
    do
        rng <- getSplit

        let adef = AgentDef {
          adId = aid,
          adState = AuctioneerState,   -- NOTE: again, the auctioneer does not has any domain-specific state
          adConversation = Nothing,
          adInitMessages = NoEvent,
          adBeh = auctioneerBehaviour,
          adRng = rng
        }

        return adef