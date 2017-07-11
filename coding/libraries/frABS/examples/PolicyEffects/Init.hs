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

createPolicyEffects :: (Int, Int) 
                        -> Double 
                        -> NetworkType
                        -> IO ([PolicyEffectsAgentDef], PolicyEffectsEnvironment)
createPolicyEffects dims@(maxX, maxY) initWealth network =  
    do
        rng <- newStdGen
        env <- evalRandIO $ createRandEnvironment dims network

        let gr = envGraph env
        let agentIds = nodesOfNetwork gr
        adefs <- mapM (policyEffectsAgent initWealth) (zip coords agentIds)

        return (adefs, env)

    where
        coords = [ (x, y) | x <- [0..maxX - 1], y <- [0..maxY - 1]]

policyEffectsAgent :: Double
                        -> (EnvCoord, AgentId)
                        -> IO PolicyEffectsAgentDef
policyEffectsAgent initWealth (coord, agentId) = 
    do
        rng <- newStdGen

        return AgentDef { adId = agentId,
                            adState = initWealth,
                            adBeh = policyEffectsAgentBehaviour,
                            adInitMessages = NoEvent,
                            adConversation = Nothing,
                            adEnvPos = coord,
                            adRng = rng }

createRandEnvironment :: (Int, Int) -> NetworkType -> Rand StdGen PolicyEffectsEnvironment
createRandEnvironment dims network =
    do
        rng <- getSplit
        gr <- createAgentNetwork network

        return $ createEnvironment
                        Nothing
                        dims
                        moore
                        ClipToMax
                        []
                        rng
                        (Just gr)

policyEffectsEnvReplicator :: (Int, Int) -> NetworkType -> PolicyEffectsEnvironmentReplicator
policyEffectsEnvReplicator dims network rng env = runRand (createRandEnvironment dims network) rng
------------------------------------------------------------------------------------------------------------------------