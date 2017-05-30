{-# LANGUAGE Arrows #-}
module FrABS.Simulation.Simulation where

-- Project-internal import first
import FrABS.Env.Environment
import FrABS.Simulation.SeqIteration
import FrABS.Simulation.ParIteration
import FrABS.Agent.Agent
import FrABS.Simulation.Utils

-- Project-specific libraries follow
import FRP.Yampa
import FRP.Yampa.InternalCore

-- System imports then
import Data.Maybe
import Data.List

-- TODO: remove these imports
-- debugging imports finally, to be easily removed in final version
import Debug.Trace

type EnvironmentCollapsing ec = ([Environment ec] -> Environment ec)

------------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
------------------------------------------------------------------------------------------------------------------------
-- NOTE: don't care about a, we don't use it anyway
processIOInit :: [AgentDef s m ec]
                    -> Environment ec
                    -> Maybe (EnvironmentCollapsing ec)
                    -> (ReactHandle ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec)
                            -> Bool
                            -> ([AgentOut s m ec], Environment ec)
                            -> IO Bool)
                    -> IO (ReactHandle ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec))
processIOInit as env mayParCollapsing iterFunc = reactInit
                                                (return (ains, env))
                                                iterFunc
                                                (process as mayParCollapsing)
    where
        ains = createStartingAgentIn as env
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CALCULATING A FIXED NUMBER OF STEPS OF THE SIMULATION
------------------------------------------------------------------------------------------------------------------------
{- NOTE: to run Yampa in a pure-functional way use embed -}
processSteps :: [AgentDef s m ec]
                    -> Environment ec
                    -> Maybe (EnvironmentCollapsing ec)
                    -> Double
                    -> Int
                    -> [([AgentOut s m ec], Environment ec)]
processSteps as env mayParCollapsing dt steps = embed
                                            (process as mayParCollapsing)
                                            ((ains, env), sts)
    where
        -- NOTE: again haskells laziness put to use: take steps items from the infinite list of sampling-times
        sts = replicate steps (dt, Nothing)
        ains = createStartingAgentIn as env
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
process :: [AgentDef s m ec]
                -> Maybe (EnvironmentCollapsing ec)
                -> SF ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec)
process as mayParCollapsing = iterationStrategy asfs mayParCollapsing
    where
        asfs = map adBeh as

iterationStrategy :: [SF (AgentIn s m ec) (AgentOut s m ec)]
                        -> Maybe (EnvironmentCollapsing ec)
                        -> SF ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec)
iterationStrategy asfs mayParCollapsing = maybe (simulateSeq asfs) (\parCollapsing -> simulatePar asfs parCollapsing) mayParCollapsing


simulate :: ([AgentIn s m ec], Environment ec)
                  -> [SF (AgentIn s m ec) (AgentOut s m ec)]
                  -> Maybe (EnvironmentCollapsing ec)
                  -> Double
                  -> Int
                  -> [([AgentOut s m ec], Environment ec)]
simulate ains asfs mayParCollapsing dt steps = embed
                                                  sfStrat
                                                  (ains, sts)
    where
        sts = replicate steps (dt, Nothing)
        sfStrat = iterationStrategy asfs mayParCollapsing
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- SEQUENTIAL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulateSeq :: [SF (AgentIn s m ec) (AgentOut s m ec)]
                -> SF ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec)
simulateSeq initSfs = SF {sfTF = tf0}
    where
        tf0 (initInputs, initEnv) = (tfCont, ([], initEnv))
            where
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk 0.0
                -- NOTE: in SEQ we need already to know the dt for the NEXT step because we are iterating in sequence => ommit first output => need 1 step more
                tfCont = simulateSeqAux initSfs initInputs initEnv

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        simulateSeqAux sfs ins env = SF' tf
            where
                -- NOTE: this is a function definition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ = (tf', (outs, env''))
                    where
                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs', ins', outs) = runSeqInternal sfs ins seqCallback seqCallbackIteration dt

                        -- NOTE: the 'last' environment is in the first of outs because runSeqInternal reverses the outputs
                        env' = if null outs then env else aoEnv $ head outs
                        env'' = runEnv env' dt

                        insWithNewEnv = map (\ain -> ain { aiEnv = env'' }) ins'

                        -- create a continuation of this SF
                        tf' = simulateSeqAux sfs' insWithNewEnv env''

seqCallbackIteration :: [AgentOut s m ec] -> ([AgentBehaviour s m ec], [AgentIn s m ec])
seqCallbackIteration aouts = (newSfs, newSfsIns')
    where
        -- NOTE: messages of this agent are ALWAYS distributed, whether it is killed or not
        (newSfs, newSfsIns) = foldr handleCreateAgents ([], []) aouts
        -- NOTE: distribute messages to newly created agents as well
        newSfsIns' = distributeMessages newSfsIns aouts

seqCallback :: ([AgentIn s m ec], [AgentBehaviour s m ec])
                -> (AgentBehaviour s m ec)
                -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                -> ([AgentIn s m ec],
                    Maybe (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec))
seqCallback (otherIns, otherSfs) oldSf (sf, oldIn, newOut)
    | doRecursion = seqCallbackRec otherIns otherSfs oldSf (sf, recIn, newOut)
    | otherwise = handleAgent otherIns (sf, unRecIn, newOut)
    where
        -- NOTE: first layer of recursion: calling simulate within a simulation
        -- NOTE: at this level we are determining how many levels of recursion we run: at the moment, we stop after the first level
        doRecursion = if (isEvent (aiRec oldIn)) then
                        False   -- this is recursion-level 1 (0 is the initial level), stop here, can be replaced by counter in the future
                        else
                            isEvent $ aoRec newOut      -- this is recursion-level 0 (initial level), do recursion only if the agent requests to do so

        -- NOTE: need to handle inputs different based upon whether we are doing
        recIn = if (isEvent $ aiRec oldIn) then
                    oldIn -- this is recursion level 1 => will do normal agent-handling and feed past outputs to the agent so it can select the best
                    else
                        oldIn { aiRec = Event [] } -- this is recursion level 0 => start with empty past outputs

        -- NOTE: need to stop recursion
        unRecIn = oldIn { aiRec = NoEvent }

        -- NOTE: second layer of recursion: this allows the agent to simulate an arbitrary number of AgentOuts
        seqCallbackRec :: [AgentIn s m ec]
                           -> [AgentBehaviour s m ec]
                           -> (AgentBehaviour s m ec)
                           -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                           -> ([AgentIn s m ec],
                               Maybe (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec))
        seqCallbackRec otherIns otherSfs oldSf (sf, recIn, newOut)
            | isEvent $ aoRec newOut = handleRecursion otherIns otherSfs oldSf (sf, recIn', newOut)     -- the last output requested recursion, perform it
            | otherwise = handleAgent otherIns (sf, unRecIn, newOut)                                                     -- no more recursion request, just handle agent as it is and return it, this will transport it back to the outer level
            where
                pastOutputs = fromEvent $ aiRec recIn                           -- at this point we can be sure that there MUST be an aiRec - Event otherwise would make no sense: having an aiRec - Event with a list means, that we are inside a recursion level (either 0 or 1)
                recIn' = recIn { aiRec = Event (newOut : pastOutputs) }         -- append the new output to the past ones

                -- NOTE: need to stop recursion
                unRecIn = recIn { aiRec = NoEvent }

        -- this initiates the recursive simulation call
        handleRecursion :: [AgentIn s m ec]     -- the inputs to the 'other' agents
                                 -> [AgentBehaviour s m ec] -- the signal functions of the 'other' agents
                                 -> (AgentBehaviour s m ec)     -- the OLD signal function of the current agent: it is the SF BEFORE having initiated the recursion
                                 -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                 -> ([AgentIn s m ec],
                                        Maybe (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec))
        handleRecursion otherIns otherSfs oldSf a@(sf, oldIn, newOut)
            | isJust mayAgent = retAfterRec
            | otherwise = retSelfKilled       -- the agent killed itself, terminate recursion
            where
                -- NOTE: collect self-messages for agent, distribute its messages and environment to others
                retSelfKilled@(otherIns', mayAgent) = handleAgent otherIns a

                (_, newIn, _) = fromJust mayAgent

                otherIns'' = if allowsRecOthers newOut then otherIns' else forbidRecursion otherIns'

                -- TODO: to prevent an endless creation of recursions when running a recursion for more than 1 step one needs to let the recursive agent let know it is inside its own recursion with the same mechanism as letting others now they are inside another recursion.

                -- NOTE: need to add agent, because not included
                allAsfs = otherSfs ++ [oldSf]       -- NOTE: use the old sf, no time
                allAins = otherIns'' ++ [newIn]

                env = aoEnv newOut

                -- TODO: does it make sense to run multiple steps? what is the meaning of it?
                -- TODO: when running for multiple steps it makes sense to specify WHEN the agent of oldSF runs
                -- NOTE: when setting steps to > 1 we end up in an infinite loop
                -- TODO: only running in sequential for now
                allStepsRecOuts = (simulate (allAins, env) allAsfs Nothing 1.0 1) 

                (lastStepRecOuts, _) = (last allStepsRecOuts)
                mayRecOut = Data.List.find (\ao -> (aoId ao) == (aiId oldIn)) lastStepRecOuts

                -- TODO: what happens to the environment? it could have changed by the other agents but we need to re-set it to before
                
                -- TODO: the agent died in the recursive simulation, what should we do?
                retAfterRec = if isJust mayRecOut then
                                seqCallbackRec otherIns otherSfs oldSf (sf, newIn, fromJust mayRecOut)
                                else
                                    retSelfKilled

        forbidRecursion :: [AgentIn s m ec] -> [AgentIn s m ec]
        forbidRecursion ains = map (\ai -> ai { aiRecInitAllowed = False } ) ains

        handleAgent :: [AgentIn s m ec]
                                -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                -> ([AgentIn s m ec],
                                     Maybe (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec))
        handleAgent otherIns a@(sf, oldIn, newOut) = (otherIns'', mayAgent)
            where
                (otherIns', newOut') = handleConversation otherIns newOut
                mayAgent = handleKillOrLiveAgent (sf, oldIn, newOut')
                otherIns'' = distributeActions otherIns' newOut'

        handleConversation :: [AgentIn s m ec]
                                -> AgentOut s m ec
                                -> ([AgentIn s m ec], AgentOut s m ec)
        handleConversation otherIns newOut
            | hasConversation newOut = handleConversation otherIns' newOut'
            | otherwise = (otherIns, newOut)
            where
                conv@(_, senderReplyFunc) = fromEvent $ aoConversation newOut

                -- NOTE: it is possible that agents which are just newly created are already target of a conversation because
                --       their position in the environment was occupied using their id which exposes them to potential messages
                --       and conversations. These newly created agents are not yet available in the current iteration and can
                --       only fully participate in the next one. Thus we ignore conversation-requests

                mayRepl = conversationReply otherIns newOut conv
                (otherIns', newOut') = maybe (otherIns, senderReplyFunc newOut Nothing) id mayRepl

                conversationReply :: [AgentIn s m ec] 
                                        -> AgentOut s m ec
                                        -> (AgentMessage m, AgentConversationSender s m ec)
                                        -> Maybe ([AgentIn s m ec], AgentOut s m ec) 
                conversationReply otherIns newOut ((receiverId, receiverMsg), senderReplyFunc) =
                    do
                        receivingIdx <- findIndex ((==receiverId) . aiId) otherIns
                        let receivingIn = otherIns !! receivingIdx 
                        convHandler <- aiConversation receivingIn
                        (replyM, receivingIn') <- convHandler receivingIn (aoId newOut, receiverMsg)
                        let otherIns' = replace receivingIdx otherIns receivingIn'
                        let newOut' = senderReplyFunc newOut (Just (receiverId, replyM))
                        return (otherIns', newOut')

        replace :: Int -> [a] -> a -> [a]
        replace idx as a = front ++ (a : backNoElem)
            where
                (front, back) = splitAt idx as  -- NOTE: back includes the element with the index
                backNoElem = tail back

        handleKillOrLiveAgent :: (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                    -> Maybe (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
        handleKillOrLiveAgent (sf, oldIn, newOut)
            | killAgent = Nothing
            | otherwise = Just (sf, newIn', newOut)
            where
                killAgent = isEvent $ aoKill newOut
                newIn = oldIn { aiStart = NoEvent,
                                aiState = (aoState newOut),
                                aiMessages = NoEvent,
                                aiEnvPos = (aoEnvPos newOut),
                                aiEnv = (aoEnv newOut),
                                aiRng = aoRng newOut }

                -- NOTE: need to handle sending messages to itself because the input of this agent is not in the list of all inputs because it will be replaced anyway by newIn
                newIn' = collectMessagesFor [newOut] newIn

        distributeActions :: [AgentIn s m ec] -> AgentOut s m ec -> [AgentIn s m ec]
        distributeActions otherIns newOut = otherIns1
            where
                 -- NOTE: distribute messages to all other agents
                otherIns0 = distributeMessages otherIns [newOut]
                -- NOTE: passing the changed environment to the next agents
                otherIns1 = passEnvForward newOut otherIns0

        passEnvForward :: AgentOut s m ec -> [AgentIn s m ec] -> [AgentIn s m ec]
        passEnvForward out allIns = map (\ain -> ain { aiEnv = env }) allIns
            where
                env = aoEnv out
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- PARALLEL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulatePar :: [SF (AgentIn s m ec) (AgentOut s m ec)]
                -> EnvironmentCollapsing ec
                -> SF ([AgentIn s m ec], Environment ec) ([AgentOut s m ec], Environment ec)
simulatePar initSfs envCollapsing = SF {sfTF = tf0}
    where
        tf0 (initInputs, initEnv) = (tfCont, (initOs, initEnv))
            where
                (nextSfs, initOs) = runParInternal initSfs initInputs
                tfCont = simulateParAux nextSfs initInputs initOs initEnv

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        simulateParAux sfs ins outs env = SF' tf
            where
                -- NOTE: this is a function defition
                -- tf :: DTime -> [i] -> Transition [i] [o]
                tf dt _ =  (tf', (outs', env'))
                    where
                        -- freezing the collection of SF' to 'promote' them back to SF
                        frozenSfs = freezeCol sfs dt

                        -- using the callback to create the next inputs and allow changing of the SF-collection
                        (sfs', ins') = parCallback ins outs frozenSfs

                        insWithEnv = map (\i -> i {aiEnv = env}) ins'

                        -- run the next step with the new sfs and inputs to get the sf-contintuations and their outputs
                        (sfs'', outs') = runParInternal sfs' insWithEnv

                        allEnvs = map aoEnv outs'
                        collapsedEnv = envCollapsing allEnvs 
                        env' = runEnv collapsedEnv dt

                        -- create a continuation of this SF
                        tf' = simulateParAux sfs'' insWithEnv outs' env'


parCallback :: [AgentIn s m ec]
                -> [AgentOut s m ec]
                -> [AgentBehaviour s m ec]
                -> ([AgentBehaviour s m ec], [AgentIn s m ec])
parCallback oldAgentIns newAgentOuts asfs = (asfs', newAgentIns0)
    where
        (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
        newAgentIns0 = distributeMessages newAgentIns newAgentOuts

        processAgents :: [AgentBehaviour s m ec]
                            -> [AgentIn s m ec]
                            -> [AgentOut s m ec]
                            -> ([AgentBehaviour s m ec], [AgentIn s m ec])
        processAgents asfs oldIs newOs = foldr (\a acc -> handleAgent acc a ) ([], []) asfsWithIsOs
            where
                asfsWithIsOs = zip3 asfs oldIs newOs

                handleAgent :: ([AgentBehaviour s m ec], [AgentIn s m ec])
                                -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                -> ([AgentBehaviour s m ec], [AgentIn s m ec])
                handleAgent acc a@(_, _, newOut) = handleKillOrLiveAgent acc' a
                    where
                        acc' = handleCreateAgents newOut acc

                handleKillOrLiveAgent :: ([AgentBehaviour s m ec], [AgentIn s m ec])
                                            -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                            -> ([AgentBehaviour s m ec], [AgentIn s m ec])
                handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
                    | killAgent = acc
                    | otherwise = (asfsAcc ++ [sf], ainsAcc ++ [newIn])
                    where
                        killAgent = isEvent $ aoKill newOut
                        newIn = oldIn { aiStart = NoEvent,
                                        aiState = (aoState newOut),
                                        aiMessages = NoEvent,
                                        aiEnvPos = (aoEnvPos newOut),
                                        aiEnv = (aoEnv newOut),
                                        aiRng = aoRng newOut }
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- utils
----------------------------------------------------------------------------------------------------------------------
runEnv :: Environment c -> DTime -> Environment c
runEnv env dt
    | isNothing mayEnvBeh = env
    | otherwise = env' { envBehaviour = Just envSF' }
    where
        mayEnvBeh = envBehaviour env

        envSF = fromJust mayEnvBeh
        (envSF', env') = runAndFreezeSF envSF env dt

handleCreateAgents :: AgentOut s m ec
                        -> ([AgentBehaviour s m ec], [AgentIn s m ec])
                        -> ([AgentBehaviour s m ec], [AgentIn s m ec])
handleCreateAgents o acc@(asfsAcc, ainsAcc) 
    | hasCreateAgents = (asfsAcc ++ newSfs, ainsAcc ++ newAis)
    | otherwise = acc
    where
        newAgentInheritedEnvironment = aoEnv o
        newAgentDefsEvt = aoCreate o
        hasCreateAgents = isEvent newAgentDefsEvt
        newAgentDefs = fromEvent newAgentDefsEvt
        newSfs = map adBeh newAgentDefs
        newAis = map (startingAgentInFromAgentDef newAgentInheritedEnvironment) newAgentDefs

distributeMessages :: [AgentIn s m ec] -> [AgentOut s m ec] -> [AgentIn s m ec]
distributeMessages ains aouts = map (collectMessagesFor aouts) ains

collectMessagesFor :: [AgentOut s m ec] -> AgentIn s m ec -> AgentIn s m ec
collectMessagesFor aouts ai = ai { aiMessages = msgsEvt }
    where
        aid = aiId ai
        aiMsgs = aiMessages ai
        msgsEvt = foldr (\ao accMsgs -> mergeMessages (collectMessagesFrom aid ao) accMsgs) aiMsgs aouts

collectMessagesFrom :: AgentId -> AgentOut s m ec -> Event [AgentMessage m]
collectMessagesFrom aid ao = foldr (\(receiverId, m) accMsgs-> if receiverId == aid then
                                                                mergeMessages (Event [(senderId, m)]) accMsgs
                                                                else
                                                                    accMsgs) NoEvent msgs
    where
        senderId = aoId ao
        msgsEvt = aoMessages ao
        msgs = if isEvent msgsEvt then
                    fromEvent msgsEvt
                    else
                        []
----------------------------------------------------------------------------------------------------------------------