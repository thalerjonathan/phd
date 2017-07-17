module FRP.FrABS.Simulation.Simulation (
    UpdateStrategy (..),
    EnvironmentCollapsing,
    SimulationParams (..),

    processIOInit,
    processSteps
  ) where

import FRP.FrABS.Simulation.SeqIteration
import FRP.FrABS.Simulation.ParIteration
import FRP.FrABS.Agent.Agent
import FRP.FrABS.Simulation.Internal
import FRP.FrABS.Environment.Definitions

import FRP.Yampa
import FRP.Yampa.InternalCore

import Data.Maybe
import Data.List
import System.Random
import qualified Data.Map as Map
import Control.Concurrent.STM.TVar

data UpdateStrategy = Sequential | Parallel deriving (Eq)

data SimulationParams e = SimulationParams {
    simStrategy :: UpdateStrategy,
    simEnvBehaviour :: Maybe (EnvironmentBehaviour e),
    simEnvCollapse :: Maybe (EnvironmentCollapsing e),
    simShuffleAgents :: Bool,
    simRng :: StdGen,
    simIdGen :: TVar Int
}

------------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
------------------------------------------------------------------------------------------------------------------------
processIOInit :: [AgentDef s m e]
                    -> e
                    -> SimulationParams e
                    -> (ReactHandle ([AgentIn s m e], e) ([AgentOut s m e], e)
                            -> Bool
                            -> ([AgentOut s m e], e)
                            -> IO Bool)
                    -> IO (ReactHandle ([AgentIn s m e], e) ([AgentOut s m e], e))
processIOInit adefs e params iterFunc = reactInit
                                                (return (ains, e))
                                                iterFunc
                                                (process adefs params)
    where
        idGen = simIdGen params
        ains = createStartingAgentIn adefs idGen
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CALCULATING A FIXED NUMBER OF STEPS OF THE SIMULATION
------------------------------------------------------------------------------------------------------------------------
processSteps :: [AgentDef s m e]
                -> e
                -> SimulationParams e
                -> Double
                -> Int
                -> [([AgentOut s m e], e)]
processSteps adefs e params dt steps = embed
                                            (process adefs params)
                                            ((ains, e), sts)
    where
        sts = replicate steps (dt, Nothing)
        idGen = simIdGen params
        ains = createStartingAgentIn adefs idGen
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
process :: [AgentDef s m e]
            -> SimulationParams e
            -> SF ([AgentIn s m e], e) ([AgentOut s m e], e)
process adefs params = iterationStrategy asfs params
    where
        asfs = map adBeh adefs

simulate :: ([AgentIn s m e], e)
            -> [AgentBehaviour s m e]
            -> SimulationParams e
            -> Double
            -> Int
            -> [([AgentOut s m e], e)]
simulate ains asfs params dt steps = embed sfStrat (ains, sts)
    where
        sts = replicate steps (dt, Nothing)
        sfStrat = iterationStrategy asfs params

iterationStrategy :: [AgentBehaviour s m e]
                        -> SimulationParams e
                        -> SF ([AgentIn s m e], e) ([AgentOut s m e], e)
iterationStrategy asfs params 
    | Sequential == strategy = simulateSeq asfs params
    | Parallel == strategy = simulatePar asfs params
    where
        strategy = simStrategy params
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- SEQUENTIAL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulateSeq :: [AgentBehaviour s m e]
                -> SimulationParams e
                -> SF ([AgentIn s m e], e) ([AgentOut s m e], e)
simulateSeq initSfs initParams = SF {sfTF = tf0}
    where
        tf0 (initInputs, initEnv) = (tfCont, (initOuts, initEnv))
            where
                -- NOTE: to prevent undefined outputs we create outputs based on the initials
                initOuts = map agentOutFromIn initInputs
                --(nextSfs, initOs, nextIns) = runSeqInternal initSfs initInput clbk 0.0
                -- NOTE: in SEQ we need already to know the dt for the NEXT step because we are iterating in sequence => ommit first output => need 1 step more
                initInputsWithEnv = addEnvToAins initEnv initInputs
                tfCont = simulateSeqAux initParams initSfs initInputsWithEnv initEnv

        -- NOTE: here we create recursively a new continuation
        -- ins are the old inputs from which outs resulted, together with their sfs
        -- simulateSeqAux :: -> SimulationParams e -> [AgentBehaviour s m e] -> [(AgentIn s m e, e)] -> e -> ?
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

                        insWithNewEnv = map (\(ain, _) -> (ain, finalEnvAfterRun)) insWithEnv'

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
                allStepsRecOuts = simulate (map fst allAins, env) allAsfs params 1.0 1

                (lastStepRecOuts, lastStepEnv) = last allStepsRecOuts
                mayRecOut = Data.List.find (\ao -> (aoId ao) == (aiId $ fst oldIn)) lastStepRecOuts

                -- TODO: what happens to the environment? it could have changed by the other agents but we need to re-set it to before
                
                -- TODO: the agent died in the recursive simulation, what should we do?
                retAfterRec = if isJust mayRecOut then
                                seqCallbackRec params otherIns otherSfs oldSf (sf, newIn, (fromJust mayRecOut, lastStepEnv))
                                else
                                    retSelfKilled

        forbidRecursion :: [(AgentIn s m e, e)] -> [(AgentIn s m e, e)]
        forbidRecursion ains = map (\(ai, e) -> (ai { aiRecInitAllowed = False }, e)) ains

        handleAgent :: [(AgentIn s m e, e)]
                        -> (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e))
                        -> ([(AgentIn s m e, e)],
                             Maybe (AgentBehaviour s m e, (AgentIn s m e, e), (AgentOut s m e, e)))
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

----------------------------------------------------------------------------------------------------------------------
-- PARALLEL STRATEGY
----------------------------------------------------------------------------------------------------------------------
simulatePar :: [AgentBehaviour s m e]
                -> SimulationParams e
                -> SF ([AgentIn s m e], e) ([AgentOut s m e], e)
simulatePar initSfs initParams = SF {sfTF = tf0}
    where
        tf0 (initInputs, initEnv) = (tfCont, (initOuts, initEnv))
            where
                -- NOTE: to prevent undefined outputs we create outputs based on the initials
                initOuts = map agentOutFromIn initInputs
                initInputsWithEnv = addEnvToAins initEnv initInputs
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
newAgentIn :: (AgentIn s m e, e) -> (AgentOut s m e, e) -> (AgentIn s m e, e)
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

-- Taken from https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
fisherYatesShuffle :: RandomGen g => g -> [a] -> ([a], g)
fisherYatesShuffle gen [] = ([], gen)
fisherYatesShuffle gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)

    fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
    fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
      where
        (j, gen') = randomR (0, i) gen

addEnvToAins :: e -> [AgentIn s m e] -> [(AgentIn s m e, e)]
addEnvToAins e ains = map (swap . ((,) e)) ains

replaceEnvOfAins :: e ->  [(AgentIn s m e, e)] -> [(AgentIn s m e, e)]
replaceEnvOfAins e ains = map (swap . ((,) e) . fst) ains

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)
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
        newAisWithEnv = map (\ain -> (ain, e)) newAis

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
                mayReceiverMsgs = Map.lookup receiverId allMsgs

                msgsEvt = maybe NoEvent (\receiverMsgs -> Event receiverMsgs) mayReceiverMsgs

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