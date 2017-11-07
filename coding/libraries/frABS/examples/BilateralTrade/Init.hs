module BilateralTrade.Init (
      initBilateralTrade
  ) where

import FRP.FrABS
import FRP.Yampa
import Control.Monad.Random

import BilateralTrade.Model


initBilateralTrade :: Rand StdGen ([BTAgentDef], BTEnvironment)
initBilateralTrade = undefined

createBTAgentDef :: AgentId -> Rand StdGen BTAgentDef
createBTAgentDef aid = do
  rng <- getSplit

  initPrices <- getRandomRs (0, 1)

  let s = BTAgentState {
      prices = take goods initPrices
    , score = 0
  }

  --let beh = sirAgentBehaviour rng initS
  
  let adef = AgentDef { 
        adId = aid
      , adState = s
      --, adBeh = beh
      , adInitMessages = NoEvent
      , adConversation = Nothing
      , adRng = rng 
      }

  return adef