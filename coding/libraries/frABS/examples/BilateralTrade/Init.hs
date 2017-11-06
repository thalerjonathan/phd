module BilateralTrade.Init (
      initBilateralTrade
  ) where

import Control.Monad.Random

import BilateralTrade.Model

initBilateralTrade :: Int -> Rand StdGen ([BTAgentDef], BTEnvironment)
initBilateralTrade agentCount = undefined

createBTAgentDef :: AgentId -> Rand StdGen BTAgentDef
createBTAgentDef aid = do
  rng <- getSplit

  initPrices <- getRandomRs (0, 1)

  let s = BTAgentState {
      prices = take goods initPrices
    , score = 0
  }

  let beh = sirAgentBehaviour rng initS
  
  let adef = AgentDef { 
        adId = aid
      , adState = s
      , adBeh = beh
      , adInitMessages = NoEvent
      , adConversation = Nothing
      , adRng = rng 
      }

  return adef