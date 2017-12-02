module PolicyEffects.Init (
    createPolicyEffects,

    policyEffectsEnvReplicator
  ) where

import PolicyEffects.Model
import PolicyEffects.Agent

import FRP.Yampa

import FRP.FrABS

import System.Random
import Control.Monad.Random

createPolicyEffects :: Double 
                        -> NetworkType
                        -> IO ([PolicyEffectsAgentDef], PolicyEffectsEnvironment)
createPolicyEffects initWealth network =  
    do
        e <- evalRandIO $ createRandEnvironment network

        let agentIds = nodesOfNetwork e

        adefs <- mapM (policyEffectsAgent initWealth) agentIds

        return (adefs, e)

policyEffectsAgent :: Double
                        -> AgentId
                        -> IO PolicyEffectsAgentDef
policyEffectsAgent initWealth agentId = 
    do
        rng <- newStdGen
        return AgentDef { adId = agentId,
                            adState = initWealth,
                            adBeh = policyEffectsAgentBehaviour,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adRng = rng }

createRandEnvironment :: NetworkType -> Rand StdGen PolicyEffectsEnvironment
createRandEnvironment network = createNetwork network unitEdgeLabeler

policyEffectsEnvReplicator :: NetworkType -> PolicyEffectsEnvironmentReplicator
policyEffectsEnvReplicator network rng env = runRand (createRandEnvironment network) rng
------------------------------------------------------------------------------------------------------------------------