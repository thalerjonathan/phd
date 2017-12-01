{-# LANGUAGE Arrows #-}
{-# LANGUAGE Rank2Types #-}
module FRP.FrABS.Simulation.ParIteration 
  (
    simulatePar
  ) where

import qualified Data.Map as Map
import Data.Maybe
import Control.Concurrent.STM.TVar
import Control.Parallel.Strategies

import FRP.Yampa
import FRP.Yampa.InternalCore

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Simulation.Common

-- TODO: replace simulatePar with yampa pSwitch instead of own SF

-- | Steps the simulation using a parallel update-strategy. 
-- Conversations and Recursive Simulation is NOT possible using this strategy.
-- In this strategy each agents SF is run after the same time, actions 
-- are only seen in the next step. This makes
-- this strategy work basically as a map (as opposed to fold in the sequential case).
-- Although the agents make the move at the same time, when shuffling them, 
-- the order of collecting and distributing the messages makes a difference 
-- if model-semantics are relying on randomized message-ordering, 
-- then shuffling is required and has to be turned on in the params.
--
-- An agent which kills itself will still have all its output processed
-- meaning that newly created agents and sent messages are not discharged.
--
-- It is not possible to send messages to currently non-existing agents,
-- also not to agents which may exist in the future. Messages which
-- have as receiver a non-existing agent are discharged without any notice
-- (a minor exception is the sending of messages to newly spawned agents
-- within the iteration when they were created: although they are not running
-- yet, they are known already to the system and will run in the next step).

-- NOTE: it was decided that we provide our own implementation as it was not possible to implement these
-- iterations with Yampas existing primitives
simulatePar :: SimulationParams e
                -> [AgentBehaviour s m e]
                -> [AgentIn s m e]
                -> e
                -> SF () (SimulationStepOut s e)
simulatePar initParams initSfs initIns initEnv = SF { sfTF = tf0 }
  where
    tf0 _ = (tfCont, (initTime, [], initEnv))
      where
        initTime = 0

        tfCont = simulateParAux initParams initSfs initIns initEnv initTime

        simulateParAux params sfs ins e t = SF' tf
          where
            tf dt _ =  (tf', (t', obs, e'))
              where
                -- accumulate global simulation-time
                t' = t + dt
                -- iterate agents in parallel
                (sfs', outs, envs) = iterateAgents dt sfs ins e
                -- create next inputs and sfs (distribute messages and add/remove new/killed agents)
                (sfs'', ins') = nextStep ins outs sfs'
                -- collapse all environments into one
                (e', params') = foldEnvironments dt params envs e
                -- create observable outputs
                obs = observableAgents (map aiId ins) outs
                -- NOTE: shuffling may seem strange in parallel but this will ensure random message-distribution when required
                (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs'' ins'
                -- create continuation
                tf' = simulateParAux params'' sfsShuffled insShuffled e' t'

foldEnvironments :: Double 
                    -> SimulationParams e 
                    -> [e] 
                    -> e 
                    -> (e, SimulationParams e)
foldEnvironments dt params allEnvs defaultEnv
    | isFoldingEnv = runEnv dt params foldedEnv 
    | otherwise = (defaultEnv, params)
  where
    isFoldingEnv = isJust mayEnvFoldFun

    mayEnvFoldFun = simEnvFold params
    envFoldFun = fromJust mayEnvFoldFun

    foldedEnv = envFoldFun allEnvs 

iterateAgents :: DTime 
                -> [AgentBehaviour s m e] 
                -> [AgentIn s m e] 
                -> e
                -> ([AgentBehaviour s m e], [AgentOut s m e], [e])
iterateAgents dt sfs ins e = unzip3 sfsOutsEnvs
  where
    -- NOTE: speedup by running in parallel (if +RTS -Nx)
    sfsOutsEnvs = parMap rpar (iterateAgentsAux e) (zip sfs ins)

    iterateAgentsAux :: e
                        -> (AgentBehaviour s m e, AgentIn s m e)
                        -> (AgentBehaviour s m e, AgentOut s m e, e)
    iterateAgentsAux e (sf, ain) = (sf', ao, e')
      where
        (sf', (ao, e')) = runAndFreezeSF sf (ain, e) dt

nextStep :: [AgentIn s m e]
            -> [AgentOut s m e]
            -> [AgentBehaviour s m e]
            -> ([AgentBehaviour s m e], [AgentIn s m e])
nextStep oldAgentIns newAgentOuts asfs = (asfs', newAgentIns')
  where
    (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
    -- NOTE: need to use oldAgentIns as each index corresponds to the agent in newAgentOuts
    newAgentOutsWithAis = map (\(ai, ao) -> (aiId ai, ao)) (zip oldAgentIns newAgentOuts) 
    newAgentIns' = distributeMessages newAgentIns newAgentOutsWithAis

    processAgents :: [AgentBehaviour s m e]
                        -> [AgentIn s m e]
                        -> [AgentOut s m e]
                        -> ([AgentBehaviour s m e], [AgentIn s m e])
    processAgents asfs oldIs newOs = foldr handleAgent ([], []) asfsIsOs
      where
        asfsIsOs = zip3 asfs oldIs newOs

        handleAgent :: (AgentBehaviour s m e, AgentIn s m e, AgentOut s m e)
                        -> ([AgentBehaviour s m e], [AgentIn s m e])
                        -> ([AgentBehaviour s m e], [AgentIn s m e])
        handleAgent a@(_, oldIn, newOut) acc = handleKillOrLiveAgent acc' a
          where
            idGen = aiIdGen oldIn
            acc' = handleCreateAgents idGen newOut acc 

        handleKillOrLiveAgent :: ([AgentBehaviour s m e], [AgentIn s m e])
                                    -> (AgentBehaviour s m e, AgentIn s m e, AgentOut s m e)
                                    -> ([AgentBehaviour s m e], [AgentIn s m e])
        handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
            | killAgent = acc
            | otherwise = (sf : asfsAcc, newIn : ainsAcc) 
          where
            killAgent = isEvent $ aoKill newOut
            newIn = newAgentIn oldIn

handleCreateAgents :: TVar Int
                        -> AgentOut s m e
                        -> ([AgentBehaviour s m e], [AgentIn s m e])
                        -> ([AgentBehaviour s m e], [AgentIn s m e])
handleCreateAgents idGen ao acc@(asfsAcc, ainsAcc) 
    | hasCreateAgents = (asfsAcc ++ newSfs, ainsAcc ++ newAis)
    | otherwise = acc
  where
    newAgentDefsEvt = aoCreate ao
    hasCreateAgents = isEvent newAgentDefsEvt
    newAgentDefs = fromEvent newAgentDefsEvt
    newSfs = map adBeh newAgentDefs
    newAis = map (startingAgentInFromAgentDef idGen) newAgentDefs

distributeMessages :: [AgentIn s m e] -> [(AgentId, AgentOut s m e)] -> [AgentIn s m e]
distributeMessages ains aouts = parMap rpar (distributeMessagesAux allMsgs) ains -- NOTE: speedup by running in parallel (if +RTS -Nx)
  where
    allMsgs = collectAllMessages aouts

    distributeMessagesAux :: Map.Map AgentId [AgentMessage m]
                                -> AgentIn s m e
                                -> AgentIn s m e
    distributeMessagesAux allMsgs ain = ain'
      where
        receiverId = aiId ain
        msgs = aiMessages ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

        mayReceiverMsgs = Map.lookup receiverId allMsgs
        msgsEvt = maybe msgs (\receiverMsgs -> mergeMessages (Event receiverMsgs) msgs) mayReceiverMsgs

        ain' = ain { aiMessages = msgsEvt }

collectAllMessages :: [(AgentId, AgentOut s m e)] -> Map.Map AgentId [AgentMessage m]
collectAllMessages aos = foldr collectAllMessagesAux Map.empty aos
  where
    collectAllMessagesAux :: (AgentId, AgentOut s m e)
                                -> Map.Map AgentId [AgentMessage m]
                                -> Map.Map AgentId [AgentMessage m]
    collectAllMessagesAux (senderId, ao) accMsgs 
        | isEvent msgsEvt = foldr collectAllMessagesAuxAux accMsgs (fromEvent msgsEvt)
        | otherwise = accMsgs
      where
        msgsEvt = aoMessages ao

        collectAllMessagesAuxAux :: AgentMessage m
                                    -> Map.Map AgentId [AgentMessage m]
                                    -> Map.Map AgentId [AgentMessage m]
        collectAllMessagesAuxAux (receiverId, m) accMsgs = accMsgs'
          where
            msg = (senderId, m)
            mayReceiverMsgs = Map.lookup receiverId accMsgs
            newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs

            -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
            accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)