module FRP.FrABS.Simulation.SeqIteration 
  (
    simulateSeq
  ) where

import Control.Concurrent.STM.TVar
import Data.Maybe
import qualified Data.Map as Map

import FRP.Yampa
import FRP.Yampa.InternalCore

import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Init
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Simulation.Common

type MessageAccumulator m   = Map.Map AgentId [AgentMessage m]
type AgentInMap s m e       = Map.Map AgentId (AgentIn s m e)

-- | Steps the simulation using a sequential update-strategy. 
-- Conversations and Recursive Simulation is only possible using this strategy.
-- In this strategy each agents SF is run after another where 
-- the actions of previous agents are seen by later ones. This makes
-- this strategy work basically as a fold (as opposed to map in the parallel case).
-- No assumption is made on the order of the iteration but agents can be shuffled 
-- to ensure a uniform distribution of the probability of being at a given position.
--
-- An agent which kills itself will still have all its output processed
-- meaning that newly created agents and sent messages are not discharged.
--
-- An agent spawned by an existing one can receive already messages
-- but will run for the first time only in the next iteration.
--
-- It is not possible to send messages to currently non-existing agents,
-- also not to agents which may exist in the future. Messages which
-- have as receiver a non-existing agent are discharged without any notice
-- (a minor exception is the sending of messages to newly spawned agents
-- within the iteration when they were created: although they are not running
-- yet, they are known already to the system and will run in the next step).
simulateSeq:: SimulationParams e
                -> [AgentBehaviour s m e]
                -> [AgentIn s m e]
                -> e
                -> SF () (SimulationStepOut s e) -- (Time, [(AgentId, AgentObservable s)], e)
simulateSeq initParams initSfs initIns initEnv = SF { sfTF = tf0 }
  where
    tf0 _ = (tfCont, (initTime, initObs, initEnv))
      where
        -- NOTE: at t = 0 there can be no observable output
        initObs = []

        initInsMap = insertIntoMap initIns Map.empty
        initAis = map aiId initIns
        initMsgs = insertEmptyEntries initAis Map.empty 
        initTime = 0

        tfCont = simulateSeqNewAux initParams initSfs initInsMap initAis initEnv initMsgs initTime

    -- NOTE: we are using a Map (Map.Map AgentId (AgentIn s m e)) to keep track of the agentins
    -- problem is that we need to update them while iterating over them where changes might only
    -- be visible in the next iteration. Thus we utilize a map which allows us to update them
    -- and keep track of agent-ids for iterating over the map. This saves us from creating the
    -- list using keys of map - also easier to keep our own order and easy shuffling.
    simulateSeqNewAux params sfs insMap ais e msgs t = SF' tf
      where
        idGen = simIdGen params

        tf dt _ = (tf', (t', obs, e'))
          where
            -- accumulate global simulation-time
            t' = t + dt
            -- iterates over the existing agents
            (sfs', outs, insMap', e', msgs') = iterateAgents dt sfs insMap ais e msgs
            -- create observable output
            -- obs = map (\(aid, ao) -> (aid, aoState ao)) (zip ais outs)
            obs = observableAgents ais outs
            -- adds/removes new/killed agents
            (sfs'', ais', insMap'') = liveKillAndSpawn idGen insMap' (zip3 sfs' ais outs)
            -- shuffles agents (if requested by params)
            (params', sfsShuffled, aisShuffled) = shuffleAgents params sfs'' ais'
            -- runs the environment (if requested by params)
            (e'', params'') = runEnv dt params' e'
            -- create the continuation
            tf' = simulateSeqNewAux params'' sfsShuffled insMap'' aisShuffled e'' msgs' t'

-- TODO: implement recursion
iterateAgents :: DTime
                  -> [AgentBehaviour s m e]
                  -> AgentInMap s m e
                  -> [AgentId]
                  -> e
                  -> MessageAccumulator m
                  -> ([AgentBehaviour s m e], [AgentOut s m e], AgentInMap s m e, e, MessageAccumulator m)
iterateAgents dt sfs insMap ais e msgs = foldr handleAgent ([], [], insMap, e, msgs) (zip sfs ais)
  where
    handleAgent :: (AgentBehaviour s m e, AgentId)
                    -> ([AgentBehaviour s m e], [AgentOut s m e], AgentInMap s m e, e, MessageAccumulator m)
                    -> ([AgentBehaviour s m e], [AgentOut s m e], AgentInMap s m e, e, MessageAccumulator m)
    handleAgent (sf, aid) (accSfs, accOuts, insMap, env, msgs) = (accSfs', accOuts', insMap', env'', msgs''')
      where
        -- NOTE: we can asume that all lookups are valid
        ain = fromJust $ Map.lookup aid insMap
        -- collecting existing messages from other agents
        ain' = collectMessages msgs ain

        (sf', (ao, env')) = runAndFreezeSF sf (ain', env) dt

        -- NOTE: handle conversation here
        (ao', env'', insMap') = handleConversation ao env' insMap

        accSfs' = sf' : accSfs
        accOuts' = ao' : accOuts

        -- NOTE: when there are new agents to spawn, add entry in messageaccumulator BEFORE distributing the messages, could send to them
        msgs' = addNewAgentsEntries ao msgs
        -- NOTE: distributing the messages to all other agents
        msgs'' = distributeMessages (aid, ao) msgs'
        -- NOTE: in case of kill delete from messageaccumulator AFTER having distributed the messages (could have sent to itself)
        msgs''' = resetOrDeleteMessages ao msgs'' 

        collectMessages :: MessageAccumulator m -> AgentIn s m e -> AgentIn s m e
        collectMessages msgAcc ain
            | isEvent msgs = ain'
            | otherwise = ain
          where
            aid = aiId ain
            msgs = maybeToEvent $ Map.lookup aid msgAcc 
            ain' = ain { aiMessages = msgs } 

        addNewAgentsEntries :: AgentOut s m e -> MessageAccumulator m -> MessageAccumulator m
        addNewAgentsEntries ao msgAcc 
            | isEvent $ aoCreate ao = insertEmptyEntries ids msgAcc
            | otherwise = msgAcc
          where
            adefs = fromEvent $ aoCreate ao
            ids = map adId adefs

        distributeMessages :: (AgentId, AgentOut s m e) -> MessageAccumulator m -> MessageAccumulator m
        distributeMessages (senderId, ao) msgAcc = event msgAcc (foldr (distributeMessageTo senderId) msgAcc) (aoMessages ao)
          where
            distributeMessageTo :: AgentId
                                    -> AgentMessage m
                                    -> MessageAccumulator m
                                    -> MessageAccumulator m
            distributeMessageTo senderId (receiverId, m) accMsgs 
                | isJust mayReceiverMsgs = accMsgs'
                | otherwise = accMsgs -- NOTE: if not found, discharge message
              where 
                mayReceiverMsgs = Map.lookup receiverId accMsgs

                msg = (senderId, m)                
                receiverMsgs = fromJust mayReceiverMsgs
                newMsgs = msg : receiverMsgs

                -- NOTE: force evaluation of messages, will reduce memory-overhead EXTREMELY
                accMsgs' = seq newMsgs (Map.insert receiverId newMsgs accMsgs)
                  
        resetOrDeleteMessages :: AgentOut s m e 
                                    -> MessageAccumulator m 
                                    -> MessageAccumulator m
        resetOrDeleteMessages ao msgAcc
            | isDead ao = Map.delete aid msgAcc
            | otherwise = emptyEntry aid msgAcc
          where
              aid = aiId ain

        handleConversation :: AgentOut s m e
              -> e
              -> AgentInMap s m e
              -> (AgentOut s m e, e, AgentInMap s m e)
        handleConversation ao e insMap = (ao, e, insMap) -- TODO: reimplement
              {-
        handleConversation :: AgentOut s m e
                              -> e
                              -> AgentInMap s m e
                              -> (AgentOut s m e, e, AgentInMap s m e)
        handleConversation ao e insMap
            | hasConversation ao = handleConversation ao' e' insMap'
            | otherwise = (ao, e, insMap)
          where
            conv@(_, senderReplyFunc) = fromEvent $ aoConversation ao

            -- NOTE: it is possible that agents which are just newly created are already target of a conversation because
            --       their position in the environment was occupied using their id which exposes them to potential messages
            --       and conversations. These newly created agents are not yet available in the current iteration and can
            --       only fully participate in the next one. Thus we ignore conversation-requests

            mayRepl = conversationReply ao e insMap conv
            (noReplyAo, noReplyEnv) = senderReplyFunc ao e Nothing
            (ao', e', insMap') = fromMaybe (noReplyAo, noReplyEnv, insMap) mayRepl

            conversationReply :: AgentOut s m e
                                    -> e
                                    -> AgentInMap s m e
                                    -> (AgentMessage m, AgentConversationSender s m e)
                                    -> Maybe (AgentOut s m e, e, AgentInMap s m e)
            conversationReply ao e insMap ((receiverId, receiverMsg), senderReplyFunc) = do
                receiver <- Map.lookup receiverId insMap
                convHandler <- aiConversation receiver
                let msg = (aoId ao, receiverMsg)
                (receiverState, replyMsg, e') <- convHandler receiver e msg
                let receiver' = receiver { aiState = receiverState }
                let insMap' = Map.insert receiverId receiver' insMap
                let (ao', e'') = senderReplyFunc ao e' (Just (receiverId, replyMsg))
                return (ao', e'', insMap')
                -}

liveKillAndSpawn :: TVar Int
                    -> AgentInMap s m e
                    -> [(AgentBehaviour s m e, AgentId, AgentOut s m e)]
                    -> ([AgentBehaviour s m e], [AgentId], AgentInMap s m e)
liveKillAndSpawn idGen insMap as = foldr (liveKillAndSpawnAux idGen) ([], [], insMap) as
  where
    liveKillAndSpawnAux :: TVar Int
                            -> (AgentBehaviour s m e, AgentId, AgentOut s m e)
                            -> ([AgentBehaviour s m e], [AgentId], AgentInMap s m e)
                            -> ([AgentBehaviour s m e], [AgentId], AgentInMap s m e)
    liveKillAndSpawnAux idGen (sf, aid, ao) (accSfs, accAis, insMap)
        | isDead ao = (accSfs', accAis', insMapDead)
        | otherwise = (sf : accSfs', aid : accAis', insMapLive)
      where
        (newSfs, newAins) = handleSpawns idGen ao

        accSfs' = accSfs ++ newSfs
        accAis' = accAis ++ (map aiId newAins)
        insMap' = insertIntoMap newAins insMap

        -- NOTE: we can asume that all lookups are valid
        ain = fromJust $ Map.lookup aid insMap
        ain' = newAgentIn ain

        insMapDead = Map.delete aid insMap'
        insMapLive = Map.insert aid ain' insMap'

    handleSpawns :: TVar Int -> AgentOut s m e -> ([AgentBehaviour s m e], [AgentIn s m e])
    handleSpawns idGen ao 
        | isEvent $ aoCreate ao = (sfs, ains)
        | otherwise = ([], [])
      where
        adefs = fromEvent $ aoCreate ao
        (sfs, ains) = startingAgent adefs idGen

insertEmptyEntries :: [AgentId] -> MessageAccumulator m -> MessageAccumulator m
insertEmptyEntries ids msgAcc = foldr emptyEntry msgAcc ids

emptyEntry :: AgentId -> MessageAccumulator m -> MessageAccumulator m
emptyEntry aid msgAcc = Map.insert aid [] msgAcc

insertIntoMap :: [AgentIn s m e] -> AgentInMap s m e -> AgentInMap s m e
insertIntoMap ais insMap = foldr (\ain acc -> Map.insert (aiId ain) ain acc) insMap ais

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just a) = Event a

{--
----------------------------------------------------------------------------------------------------------------------
-- NOTE: this is used for internal, recursive simulation and can be requested by agents in SEQUENTIAL strategy ONLY
simulateRecursive :: ([AgentIn s m e], e)
                    -> [AgentBehaviour s m e]
                    -> SimulationParams e
                    -> Double
                    -> Int
                    -> [([AgentOut s m e], e)]
simulateRecursive (ais, e) asfs params dt steps = embed sfStrat ((), sts)
    where
        sts = replicate steps (dt, Nothing)
        sfStrat = simulateSeq params asfs ais e
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- SEQUENTIAL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulateSeqOld :: SimulationParams e
                -> [AgentBehaviour s m e]
                -> [AgentIn s m e]
                -> e
                -> SF () ([AgentOut s m e], e)
simulateSeqOld initParams initSfs initAins initEnv = SF { sfTF = tf0 }
    where
        tf0 _ = (tfCont, (initOuts, initEnv))
            where
                -- NOTE: to prevent undefined outputs we create outputs based on the initials
                initOuts = map agentOutFromIn initAins
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk 0.0
                -- NOTE: in SEQ we need already to know the dt for the NEXT step because we are iterating in sequence => ommit first output => need 1 step more
                initInputsWithEnv = addEnvToAins initEnv initAins
                tfCont = simulateSeqAux initParams initSfs initInputsWithEnv initEnv

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        -- simulateSeqAux :: SimulationParams e -> [AgentBehaviour s m e] -> [(AgentIn s m e, e)] -> e -> ?
        simulateSeqAux params sfs insWithEnv e = SF' tf
            where
                -- NOTE: this is a function definition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ = (tf', (outs, finalEnvAfterRun))
                    where
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', insWithEnv', outsWithEnv) = runSeqInternal 
                            sfs 
                            insWithEnv 
                            (seqCallback params) 
                            (seqCallbackIteration $ simIdGen initParams)
                            dt

                        -- NOTE: the 'last' environment is in the first of outs because runSeqInternal reverses the outputs
                        finalEnv = if null outsWithEnv then e else snd $ head outsWithEnv
                        (finalEnvAfterRun, params') = runEnv dt params finalEnv 
                        outs = map fst outsWithEnv

                        insWithNewEnv = replaceEnvOfAins finalEnvAfterRun insWithEnv'

                        (params'', sfsShuffled, insShuffled) = shuffleAgents params' sfs' insWithNewEnv

                        -- create a continuation of this SF
                        tf' = simulateSeqAux params'' sfsShuffled insShuffled finalEnvAfterRun

seqCallbackIteration :: TVar Int 
                        -> [(AgentOut s m e, e)]
                        -> ([AgentBehaviour s m e], [(AgentIn s m e, e)])
seqCallbackIteration idGen aouts = (newSfs, newSfsIns')
    where
        -- NOTE: messages of this agent are ALWAYS distributed, whether it is killed or not
        (newSfs, newSfsIns) = foldr (handleCreateAgents idGen) ([], []) aouts
        -- NOTE: distribute messages to newly created agents as well
        newSfsIns' = distributeMessages newSfsIns aouts

seqCallback :: SimulationParams e
                -> ([(AgentIn s m e, e)], [AgentBehaviour s m e])
                -> AgentBehaviour s m e
                -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                -> ([(AgentIn s m e, e)], Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e)))
seqCallback params 
            (otherIns, otherSfs) 
            oldSf 
            (sf, (oldIn, oldInEnv), newOut)
    | doRecursion = seqCallbackRec params otherIns otherSfs oldSf (sf, recIn, newOut)
    | otherwise = handleAgent otherIns (sf, unRecIn, newOut)
    where
        -- NOTE: first layer of recursion: calling simulate within a simulation
        -- NOTE: at this level we are determining how many levels of recursion we run: at the moment, we stop after the first level
        doRecursion = if (isEvent $ aiRec oldIn) then
                        False   -- this is recursion-level 1 (0 is the initial level), stop here, can be replaced by counter in the future
                        else
                            isEvent $ aoRec $ fst newOut      -- this is recursion-level 0 (initial level), do recursion only if the agent requests to do so

        -- NOTE: need to handle inputs different based upon whether we are doing
        recIn = if (isEvent $ aiRec oldIn) then
                    (oldIn, oldInEnv) -- this is recursion level 1 => will do normal agent-handling and feed past outputs to the agent so it can select the best
                    else
                        (oldIn { aiRec = Event [] }, oldInEnv) -- this is recursion level 0 => start with empty past outputs

        -- NOTE: need to stop recursion
        unRecIn = (oldIn { aiRec = NoEvent }, oldInEnv)

        -- NOTE: second layer of recursion: this allows the agent to simulate an arbitrary number of AgentOuts
        seqCallbackRec :: SimulationParams e
                           -> [(AgentIn s m e, e)]
                           -> [AgentBehaviour s m e]
                           -> AgentBehaviour s m e
                           -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                           -> ([(AgentIn s m e, e)],
                               Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e)))
        seqCallbackRec params otherIns otherSfs oldSf (sf, (recIn, recInEnv), newOut)
            | isEvent $ aoRec $ fst newOut = handleRecursion params otherIns otherSfs oldSf (sf, recIn', newOut)     -- the last output requested recursion, perform it
            | otherwise = handleAgent otherIns (sf, unRecIn, newOut)                                                     -- no more recursion request, just handle agent as it is and return it, this will transport it back to the outer level
            where
                pastOutputs = fromEvent $ aiRec recIn                           -- at this point we can be sure that there MUST be an aiRec - Event otherwise would make no sense: having an aiRec - Event with a list means, that we are inside a recursion level (either 0 or 1)
                recIn' = (recIn { aiRec = Event (newOut : pastOutputs) }, recInEnv)         -- append the new output to the past ones

                -- NOTE: need to stop recursion
                unRecIn = (recIn { aiRec = NoEvent }, recInEnv)

        -- this initiates the recursive simulation call
        handleRecursion :: SimulationParams e
                             -> [(AgentIn s m e, e)]     -- the inputs to the 'other' agents
                             -> [AgentBehaviour s m e] -- the signal functions of the 'other' agents
                             -> AgentBehaviour s m e     -- the OLD signal function of the current agent: it is the SF BEFORE having initiated the recursion
                             -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                             -> ([(AgentIn s m e, e)],
                                    Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e)))
        handleRecursion params otherIns otherSfs oldSf a@(sf, oldIn, newOut)
            | isJust mayAgent = retAfterRec
            | otherwise = retSelfKilled       -- the agent killed itself, terminate recursion
            where
                -- NOTE: collect self-messages for agent, distribute its messages and environment to others
                retSelfKilled@(otherIns', mayAgent) = handleAgent otherIns a

                (_, newIn, _) = fromJust mayAgent

                otherIns'' = if allowsRecOthers $ fst newOut then otherIns' else forbidRecursion otherIns'

                -- TODO: to prevent an endless creation of recursions when running a recursion for more than 1 step one needs to let the recursive agent let know it is inside its own recursion with the same mechanism as letting others now they are inside another recursion.

                -- NOTE: need to add agent, because not included
                -- TODO: does it really have to be added at the end?
                allAsfs = otherSfs ++ [oldSf]       -- NOTE: use the old sf, no time
                allAins = otherIns'' ++ [newIn]

                env = snd newOut

                -- TODO: does it make sense to run multiple steps? what is the meaning of it?
                -- TODO: when running for multiple steps it makes sense to specify WHEN the agent of oldSF runs
                -- NOTE: when setting steps to > 1 we end up in an infinite loop
                -- TODO: only running in sequential for now
                allStepsRecOuts = simulateRecursive (map fst allAins, env) allAsfs params 1.0 1

                (lastStepRecOuts, lastStepRecEnv) = last allStepsRecOuts
                mayRecOut = Data.List.find (\ao -> (aoId ao) == (aiId $ fst oldIn)) lastStepRecOuts

                -- TODO: what happens to the environment? it could have changed by the other agents but we need to re-set it to before
                
                -- TODO: the agent died in the recursive simulation, what should we do?
                retAfterRec = if isJust mayRecOut then
                                seqCallbackRec params otherIns otherSfs oldSf (sf, newIn, (fromJust mayRecOut, lastStepRecEnv)) -- TODO: is this really correct to return this environment?
                                else
                                    retSelfKilled

        forbidRecursion :: [(AgentIn s m e, e)] -> [(AgentIn s m e, e)]
        forbidRecursion ains = map (\(ai, e) -> (ai { aiRecInitAllowed = False }, e)) ains

        handleAgent :: [(AgentIn s m e, e)]
                        -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                        -> ([(AgentIn s m e, e)], Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e)))
        handleAgent otherIns a@(sf, oldIn, newOut) = (otherIns'', mayAgent)
            where
                (otherIns', newOut') = handleConversation otherIns newOut
                mayAgent = handleKillOrLiveAgent (sf, oldIn, newOut')
                otherIns'' = distributeActions otherIns' newOut'

        handleConversation :: [(AgentIn s m e, e)]
                                -> (AgentOut s m e, e)
                                -> ([(AgentIn s m e, e)], (AgentOut s m e, e))
        handleConversation otherIns newOut
            | hasConversation $ fst newOut = handleConversation otherIns' newOut'
            | otherwise = (otherIns, newOut)
            where
                conv@(_, senderReplyFunc) = fromEvent $ aoConversation $ fst newOut

                -- NOTE: it is possible that agents which are just newly created are already target of a conversation because
                --       their position in the environment was occupied using their id which exposes them to potential messages
                --       and conversations. These newly created agents are not yet available in the current iteration and can
                --       only fully participate in the next one. Thus we ignore conversation-requests

                mayRepl = conversationReply otherIns newOut conv
                (otherIns', newOut') = maybe (otherIns, senderReplyFunc newOut Nothing) id mayRepl

                conversationReply :: [(AgentIn s m e, e)] 
                                        -> (AgentOut s m e, e)
                                        -> (AgentMessage m, AgentConversationSender s m e)
                                        -> Maybe ([(AgentIn s m e, e)], (AgentOut s m e, e))
                conversationReply otherIns newOut ((receiverId, receiverMsg), senderReplyFunc) =
                    do
                        receivingIdx <- findIndex ((==receiverId) . aiId . fst) otherIns
                        let receivingIn = otherIns !! receivingIdx 
                        convHandler <- aiConversation $ fst receivingIn
                        (replyM, receivingIn') <- convHandler receivingIn (aoId $ fst newOut, receiverMsg)
                        let otherIns' = replace receivingIdx otherIns receivingIn'
                        let newOut' = senderReplyFunc newOut (Just (receiverId, replyM))
                        return (otherIns', newOut')

        replace :: Int -> [a] -> a -> [a]
        replace idx as a = front ++ (a : backNoElem)
            where
                (front, back) = splitAt idx as  -- NOTE: back includes the element with the index
                backNoElem = tail back

        handleKillOrLiveAgent :: (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                                    -> Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
        handleKillOrLiveAgent (sf, oldIn, newOut)
            | killAgent = Nothing
            | otherwise = Just (sf, newIn', newOut)
            where
                killAgent = isEvent $ aoKill $ fst newOut
                newIn = newAgentIn oldIn newOut
                -- NOTE: need to handle sending messages to itself because the input of this agent is not in the list of all inputs because it will be replaced anyway by newIn
                newIn' = collectMessagesFor [newOut] newIn

        distributeActions :: [(AgentIn s m e, e)] -> (AgentOut s m e, e) -> [(AgentIn s m e, e)]
        distributeActions otherIns newOut = otherIns1
            where
                 -- NOTE: distribute messages to all other agents
                otherIns0 = distributeMessages otherIns [newOut]
                -- NOTE: passing the changed environment to the next agents
                otherIns1 = passEnvForward newOut otherIns0

        passEnvForward :: (AgentOut s m e, e) -> [(AgentIn s m e, e)] -> [(AgentIn s m e, e)]
        passEnvForward (out, e) allIns = replaceEnvOfAins e allIns
----------------------------------------------------------------------------------------------------------------------



------------------------------------------------------------------------------------------------------------------------
-- SEQ implementation
------------------------------------------------------------------------------------------------------------------------
runSeqSF :: [SF i o]
            -> (([i], [SF i o]) -> (SF i o) -> (SF i o, i, o) -> ([i], Maybe (SF i o, i, o)))
            -> ([o] -> ([SF i o], [i]))
            -> SF [i] [o]
runSeqSF initSfs clbkSeq clbkIter = SF {sfTF = tf0}
    where
        tf0 initInput = (tfCont, [])
            where
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk 0.0
                -- NOTE: in SEQ we need already to know the dt for the NEXT step because we are iterating in sequence => ommit first output => need 1 step more
                tfCont = runSeqSFAux initSfs initInput

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        runSeqSFAux sfs ins = SF' tf
            where
                -- NOTE: this is a function definition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ = (tf', outs)
                    where
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', ins', outs) = runSeqInternal sfs ins clbkSeq clbkIter dt
                        -- create a continuation of this SF
                        tf' = runSeqSFAux sfs' ins'

-- TODO: this implementation reverts the order of the agents every iteration e.g. 1st iteration: [0,1,2], 2nd iteration: [2,1,0], 3rd iteration: [0,1,2]
runSeqInternal :: [SF i o]
                -> [i]
                -> (([i], [SF i o]) -> (SF i o) -> (SF i o, i, o) -> ([i], Maybe (SF i o, i, o)))
                -> ([o] -> ([SF i o], [i]))
                -> DTime
                -> ([SF i o], [i], [o])
runSeqInternal sfs ins clbkSeq clbkIter dt = (sfs' ++ newSfs, ins' ++ newSfsIns, outs)
    where
        (sfs', ins', outs) = runSeqRec
                                sfs
                                ins
                                0
                                clbkSeq
                                dt
                                ([], [], [])
        (newSfs, newSfsIns) = clbkIter outs

        runSeqRec :: [SF i o]               -- the signal-functions to run sequentially in this iteration
                    -> [i]                  -- the inputs for each signal-function (length must be equal!)
                    -> Int                  -- index of the current position in the sequence
                    -> (([i], [SF i o])   -- the callback for a single sequential calculation
                        -> (SF i o)
                        -> (SF i o, i, o)
                        -> ([i], Maybe (SF i o, i, o)))
                    -> DTime                -- time delta since last iteration
                    -> ([SF i o],           -- the accumulator for the next sfs
                        [i],                -- the accumulator for the next inputs
                        [o])                -- the accumulator for the all outputs (including also the ones of the killed agents)
                    -> ([SF i o], [i], [o])     -- the collection of signal-functions, outputs and inputs for the next iteration: the outputs are the output from the current iteration but the inputs are for the next one

        runSeqRec [] [] _ _ _ acc = acc
        runSeqRec (sf:sfs) (i:ins) idx clbk dt acc@(accSfs, accIns, accOuts) = runSeqRec sfs ins' idx' clbk dt acc'
            where
                -- 1. run the current signal-function with the required input (i) and time-delta (dt) and freeze it (with the time-delta)
                    -- results is a frozen signal-function (type SF) and the output of the signal-function
                (sf', out) = runAndFreezeSF sf i dt

                 -- NOTE: current input i is not included because it is replaced (or ignored)
                allIns = accIns ++ ins
                allSfs = accSfs ++ sfs

                -- 2. now the callback is invoked to let the simulation handle the result of the current SF
                (allIns',           -- the new inputs, NOT including the new input of the current signal-function
                    mayCont)        -- Nothing if this signal-function should be terminated, Just (sf, newIn) if the signal-function should continue (can be replaced) with the new input
                        = clbk (allIns, allSfs) sf (sf', i, out)   -- callback is provided with a list of all the inputs (which does NOT include the current input) and
                                                        -- the frozen signal-function, old input and output of the signal-function

                -- 3. split the list of all inputs at the position we are currently at, the result are two lists:
                    -- the one infront is the input for the signal-functions in the NEXT iteration,
                    -- the one behind is the input for the signal-functions remaining in this iteration
                (accIns', ins') = splitAt idx allIns'

                acc' = accumulateData (accSfs, accIns', accOuts) mayCont out

                idx' = if isJust mayCont then idx + 1 else idx

                accumulateData :: ([SF i o], [i], [o]) -> Maybe (SF i o, i, o) -> o -> ([SF i o], [i], [o])
                accumulateData (accSfs, accIns, accOuts) mayCont oldOut
                    | isJust mayCont = (sf : accSfs, newIn : accIns, changedOut : accOuts)
                    | otherwise = (accSfs, accIns, oldOut : accOuts)
                        where
                            (sf, newIn, changedOut) = fromJust mayCont
------------------------------------------------------------------------------------------------------------------------

-}