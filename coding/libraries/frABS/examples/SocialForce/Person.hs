module SocialForce.Person (
    createPerson
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import SocialForce.Model

personStateChart :: SocialForceAgentBehaviour
personStateChart = undefined 

personUpdate :: SocialForceAgentBehaviour
personUpdate = undefined

personBehaviour :: SocialForceAgentBehaviour
personBehaviour = doRepeatedlyEvery unitTime personUpdate -- TODO: do state-chart first and then do personupdate

-------------------------------------------------------------------------------
createPerson :: AgentId
                -> Continuous2dCoord 
                -> Rand StdGen SocialForceAgentDef
createPerson aid p = do
  rng <- getSplit

  riRand <- getRandomR (0.15, 0.25)

  let s = Person {
      perPos          = p
    , perArrivedDest  = False
    , perHeading      = 0
    , perVi0          = 1.4
    , perAi           = 50 * (pre_ppl_psy ** 2) 
    , perBi           = 0.2
    , perK            = 1.2 * 100000
    , perk            = 2.4 * 100000
    , perRi           = riRand
    , perMi           = 80
    , perDest         = (0, 0)
    , perConRange     = pre_range
    , perAttRange     = pre_angle
    , perAiWall       = 50 * (pre_wall_psy ** 2)
    , perAiGrp        = 5
    , perColor        = whiteColor
    , perBelGroup     = Nothing
    , perDestScreen   = -1
  }

  return AgentDef {
    adId = aid,
    adState = s,
    adConversation = Nothing,
    adInitMessages = NoEvent,
    adBeh = personBehaviour,
    adRng = rng 
  }