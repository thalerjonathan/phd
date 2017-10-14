module FRP.FrABS.Simulation.ParIteration (
    simulatePar
  ) where

import qualified Data.Map as Map
import Data.Maybe
import Control.Concurrent.STM.TVar
import Data.Tuple

import FRP.Yampa
import FRP.Yampa.InternalCore

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Utils
import FRP.FrABS.Environment.Definitions

----------------------------------------------------------------------------------------------------------------------
-- PARALLEL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulatePar :: SimulationParams e
                -> [AgentBehaviour s m e]
                -> [AgentIn s m e]
                -> e
                -> SF () ([AgentOut s m e], e)
simulatePar initParams initSfs initAis initEnv = SF { sfTF = tf0 }
    where
        tf0 _ = (tfCont, (initOuts, initEnv))
            where
                -- NOTE: to prevent undefined outputs we create outputs based on the initials
                initOuts = map agentOutFromIn initAis
                initInputsWithEnv = addEnvToAins initEnv initAis
                tfCont = simulateParAux initParams initSfs initInputsWithEnv initEnv

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        simulateParAux params sfs insWithEnv e = SF' tf
            where
                -- NOTE: this is a function defition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ =  (tf', (outs, e'))
                    where
                         -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', outsWithEnv) = runParInternal sfs insWithEnv

                        -- freezing the collection of SF' to 'promote' them back to SF
                        frozenSfs = freezeCol sfs' dt

                        -- using the callback to create the next inputs and allow changing of the SF-collection
                        (sfs'', insWithEnv') = parCallback insWithEnv outsWithEnv frozenSfs

                        (e', params') = collapseEnvironments dt params outsWithEnv e 
                        insWithEnv'' = distributeEnvironment params' insWithEnv' e'

                        -- NOTE: although the agents make the move at the same time, when shuffling them, 
                        --          the order of collecting and distributing the messages makes a difference 
                        --          if model-semantics are relying on randomized message-ordering, then shuffling is required and has to be turned on in the params
                        (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs'' insWithEnv''
                        outs = map fst outsWithEnv

                        -- create a continuation of this SF
                        tf' = simulateParAux params'' sfsShuffled insShuffled e'

        collapseEnvironments :: Double -> SimulationParams e -> [(AgentOut s m e, e)] -> e -> (e, SimulationParams e)
        collapseEnvironments dt params outsWithEnv e
            | isCollapsingEnv = runEnv dt params collapsedEnv 
            | otherwise = (e, params)
            where
                isCollapsingEnv = isJust mayEnvCollapFun

                mayEnvCollapFun = simEnvCollapse params
                envCollapFun = fromJust mayEnvCollapFun

                allEnvs = map snd outsWithEnv
                collapsedEnv = envCollapFun allEnvs 

        distributeEnvironment :: SimulationParams e -> [(AgentIn s m e, e)] -> e -> [(AgentIn s m e, e)]
        distributeEnvironment params ins e 
            | isCollapsingEnv = replaceEnvOfAins e ins
            | otherwise = ins
            where
                isCollapsingEnv = isJust $ simEnvCollapse params

parCallback :: [(AgentIn s m e, e)]
                -> [(AgentOut s m e, e)]
                -> [AgentBehaviour s m e]
                -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
parCallback oldAgentIns newAgentOuts asfs = (asfs', newAgentIns')
    where
        (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
        newAgentIns' = distributeMessages newAgentIns newAgentOuts

        processAgents :: [AgentBehaviour s m e]
                            -> [(AgentIn s m e, e)]
                            -> [(AgentOut s m e, e)]
                            -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
        processAgents asfs oldIs newOs = foldr handleAgent ([], []) asfsIsOs
            where
                asfsIsOs = zip3 asfs oldIs newOs

                handleAgent :: (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                                -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
                                -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
                handleAgent  a@(_, oldIn, newOut) acc = handleKillOrLiveAgent acc' a
                    where
                        idGen = aiIdGen $ fst oldIn
                        acc' = handleCreateAgents idGen newOut acc 

                handleKillOrLiveAgent :: ([AgentBehaviour s m e], [(AgentIn s m e, e)])
                                            -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                                            -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
                handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
                    | killAgent = acc
                    | otherwise = (sf : asfsAcc, newIn : ainsAcc) 
                    where
                        killAgent = isEvent $ aoKill $ fst newOut
                        newIn = newAgentIn oldIn newOut
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- utils
----------------------------------------------------------------------------------------------------------------------
newAgentIn :: (AgentIn s m e, e) 
                -> (AgentOut s m e, e) 
                -> (AgentIn s m e, e)
newAgentIn (oldIn, _) (newOut, e') = (newIn, e')
    where
        newIn = oldIn { aiStart = NoEvent,
                        aiState = aoState newOut,
                        aiMessages = NoEvent,
                        aiRng = aoRng newOut }

shuffleAgents :: SimulationParams e 
                -> [AgentBehaviour s m e] 
                -> [(AgentIn s m e, e)] 
                -> (SimulationParams e, [AgentBehaviour s m e], [(AgentIn s m e, e)])
shuffleAgents params sfs ins 
    | doShuffle = (params', sfs', ins')
    | otherwise = (params, sfs, ins)
    where
        doShuffle = simShuffleAgents params
        g = simRng params 

        sfsIns = zip sfs ins
        (shuffledSfsIns, g') = fisherYatesShuffle g sfsIns

        params' = params { simRng = g' }
        --(sfs', ins') = foldr (\(sf, i) (sfAcc, insAcc) -> (sf : sfAcc, i : insAcc)) ([], []) shuffledSfsIns
        (sfs', ins') = unzip shuffledSfsIns

addEnvToAins :: e -> [AgentIn s m e] -> [(AgentIn s m e, e)]
addEnvToAins e ains = map (swap . ((,) e)) ains

replaceEnvOfAins :: e ->  [(AgentIn s m e, e)] -> [(AgentIn s m e, e)]
replaceEnvOfAins e ains = map (swap . ((,) e) . fst) ains
-----------------------------------------------------------------------------------

runEnv :: DTime -> SimulationParams e -> e -> (e, SimulationParams e)
runEnv dt params e = maybe (e, params) (runEnvAux params e) mayEnvBeh
    where
        mayEnvBeh = simEnvBehaviour params

        runEnvAux :: SimulationParams e -> e -> EnvironmentBehaviour e -> (e, SimulationParams e)
        runEnvAux params e envBeh = (e', params')
            where
                (envBeh', e') = runAndFreezeSF envBeh e dt
                params' = params { simEnvBehaviour = Just envBeh' }

handleCreateAgents :: TVar Int
                        -> (AgentOut s m e, e)
                        -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
                        -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
handleCreateAgents idGen (ao, e) acc@(asfsAcc, ainsAcc) 
    | hasCreateAgents = (asfsAcc ++ newSfs, ainsAcc ++ newAisWithEnv)
    | otherwise = acc
    where
        newAgentDefsEvt = aoCreate ao
        hasCreateAgents = isEvent newAgentDefsEvt
        newAgentDefs = fromEvent newAgentDefsEvt
        newSfs = map adBeh newAgentDefs
        newAis = map (startingAgentInFromAgentDef idGen) newAgentDefs
        newAisWithEnv = addEnvToAins e newAis

collectMessagesFor :: [(AgentOut s m e, e)] -> (AgentIn s m e, e) -> (AgentIn s m e, e)
collectMessagesFor aouts (ain, e) = (ain', e)
    where
        aid = aiId ain
        aiMsgs = aiMessages ain

        msgsEvt = foldr (\ao accMsgs -> mergeMessages (collectMessagesFrom aid ao) accMsgs) aiMsgs aouts

        ain' = ain { aiMessages = msgsEvt }

collectMessagesFrom :: AgentId -> (AgentOut s m e, e) -> Event [AgentMessage m]
collectMessagesFrom aid (ao, _) = foldr collectMessagesFromAux NoEvent msgs
    where
        senderId = aoId ao
        msgsEvt = aoMessages ao
        msgs = event [] id msgsEvt

        collectMessagesFromAux :: AgentMessage m -> Event [AgentMessage m] -> Event [AgentMessage m]
        collectMessagesFromAux (receiverId, m) accMsgs
            | receiverId == aid = mergeMessages (Event [(senderId, m)]) accMsgs
            | otherwise = accMsgs

distributeMessages = distributeMessagesFast

distributeMessagesSlow :: [(AgentIn s m e, e)] -> [(AgentOut s m e, e)] -> [(AgentIn s m e, e)]
distributeMessagesSlow ains aouts = map (collectMessagesFor aouts) ains

distributeMessagesFast :: [(AgentIn s m e, e)] -> [(AgentOut s m e, e)] -> [(AgentIn s m e, e)]
distributeMessagesFast ains aouts = map (distributeMessagesAux allMsgs) ains
    where
        allMsgs = collectAllMessages aouts

        distributeMessagesAux :: Map.Map AgentId [AgentMessage m]
                                    -> (AgentIn s m e, e)
                                    -> (AgentIn s m e, e)
        distributeMessagesAux allMsgs (ain, e) = (ain', e) 
            where
                receiverId = aiId ain
                msgs = aiMessages ain -- NOTE: ain may have already messages, they would be overridden if not incorporating them

                mayReceiverMsgs = Map.lookup receiverId allMsgs
                msgsEvt = maybe msgs (\receiverMsgs -> mergeMessages (Event receiverMsgs) msgs) mayReceiverMsgs

                ain' = ain { aiMessages = msgsEvt }

collectAllMessages :: [(AgentOut s m e, e)] -> Map.Map AgentId [AgentMessage m]
collectAllMessages aos = foldr collectAllMessagesAux Map.empty aos
    where
        collectAllMessagesAux :: (AgentOut s m e, e)
                                    -> Map.Map AgentId [AgentMessage m]
                                    -> Map.Map AgentId [AgentMessage m]
        collectAllMessagesAux (ao, _) accMsgs 
            | isEvent msgsEvt = foldr collectAllMessagesAuxAux accMsgs (fromEvent msgsEvt)
            | otherwise = accMsgs
            where
                senderId = aoId ao
                msgsEvt = aoMessages ao

                collectAllMessagesAuxAux :: AgentMessage m
                                            -> Map.Map AgentId [AgentMessage m]
                                            -> Map.Map AgentId [AgentMessage m]
                collectAllMessagesAuxAux (receiverId, m) accMsgs = Map.insert receiverId newMsgs accMsgs
                    where
                        mayReceiverMsgs = Map.lookup receiverId accMsgs
                        msg = (senderId, m)

                        newMsgs = maybe [msg] (\receiverMsgs -> (msg : receiverMsgs)) mayReceiverMsgs
----------------------------------------------------------------------------------------------------------------------

runParInternal :: [SF i o]
                    -> [i]
                    -> ([SF' i o], [o])
runParInternal sfs oldIns = (sfs', newOuts)
    where
        sfInPairs = zip sfs oldIns
        (sfs', newOuts) = foldr runSFHelper ([], []) sfInPairs

        runSFHelper :: (SF i o, i)
                        -> ([SF' i o], [o])
                        -> ([SF' i o], [o])
        runSFHelper (sf, i) (accSfs, accOs) = (sf' : accSfs, o : accOs)
            where
                (sf', o) = (sfTF sf) i
------------------------------------------------------------------------------------------------------------------------