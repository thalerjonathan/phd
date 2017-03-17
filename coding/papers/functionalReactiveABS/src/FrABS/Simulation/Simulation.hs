{-# LANGUAGE Arrows #-}

module FrABS.Simulation.Simulation where

import FrABS.Env.Environment
import FrABS.Simulation.SeqIteration
import FrABS.Simulation.ParIteration
import FrABS.Agent.Agent

import FRP.Yampa
import FRP.Yampa.InternalCore

-- TODO: remove these imports
import Debug.Trace

----------------------------------------------------------------------------------------------------------------------
-- TODOs
----------------------------------------------------------------------------------------------------------------------
-- TODO need a way to specify the environment: spatial (2d/3d continuous/discrete) or graph. includes global neighbourhood information
-- TODO  environment also needs a signalfunction which runs once per simulation-step: environmentIn/out
-- TODO agents need to be able to access the environment and update it: sequential vs parallel

-- TODO test creation / killing of agents

-- TODO random iteration in sequential

-- TODO create project structure according to put it on Hackage in september: tests, comments,...
-- TODO write unit-tests
-- TODO write QuickCheck tests

-- TODO STM FrABS using Dunai?
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION WITH ITS OWN LOOP
----------------------------------------------------------------------------------------------------------------------
processIO :: [AgentDef s m ec]
                -> Environment ec
                -> Bool
                -> ([AgentOut s m ec] -> IO (Bool, Double))
                -> IO ()
processIO as env parStrategy outFunc = do
                                        hdl <- reactInit
                                                    (return ains)
                                                    (iter outFunc)
                                                    (process as parStrategy)
                                        FrABS.Simulation.Simulation.iterate hdl (1.0, Nothing)
                                        return ()
                                            where
                                                ains = createStartingAgentIn as env

iterate :: ReactHandle a b
            -> (DTime, Maybe a)
            -> IO Bool
iterate hdl (dt, input) = do
                            cont <- react hdl (1.0, Nothing)
                            if cont then
                                FrABS.Simulation.Simulation.iterate hdl (dt, input)
                                    else
                                        return False

-- NOTE: don't care about a, we don't use it anyway
iter :: ([AgentOut s m ec]
            -> IO (Bool, Double))
            -> ReactHandle a [AgentOut s m ec]
            -> Bool
            -> [AgentOut s m ec]
            -> IO Bool
iter outFunc hdl _ out = do
                            (cont, dt) <- outFunc out
                            return cont
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- RUNNING SIMULATION FROM AN OUTER LOOP
----------------------------------------------------------------------------------------------------------------------
-- NOTE: don't care about a, we don't use it anyway
processIOInit :: [AgentDef s m ec]
                    -> Environment ec
                    -> Bool
                    -> (ReactHandle [AgentIn s m ec] [AgentOut s m ec] -> Bool -> [AgentOut s m ec] -> IO Bool)
                    -> IO (ReactHandle [AgentIn s m ec] [AgentOut s m ec])
processIOInit as env parStrategy iterFunc = reactInit
                                                (return ains)
                                                iterFunc
                                                (process as parStrategy)
    where
        ains = createStartingAgentIn as env
----------------------------------------------------------------------------------------------------------------------


----------------------------------------------------------------------------------------------------------------------
-- CALCULATING A FIXED NUMBER OF STEPS OF THE SIMULATION
----------------------------------------------------------------------------------------------------------------------
{- NOTE: to run Yampa in a pure-functional way use embed -}
processSteps :: [AgentDef s m ec]
                    -> Environment ec
                    -> Bool
                    -> Double
                    -> Int
                    -> [[AgentOut s m ec]]
processSteps as env parStrategy dt steps = embed
                                            (process as parStrategy)
                                            (ains, sts)
    where
        -- NOTE: again haskells laziness put to use: take steps items from the infinite list of sampling-times
        sts = take steps $ samplingTimes 0 dt
        ains = createStartingAgentIn as env

-- NOTE: this creates an infinite list of sampling-times with starting time t and sampling-interval dt
samplingTimes :: Double -> Double -> [(DTime, Maybe a)]
samplingTimes t dt = (t', Nothing) : (samplingTimes t' dt)
    where
        t' = t + dt
----------------------------------------------------------------------------------------------------------------------

process :: [AgentDef s m ec] -> Bool -> SF [AgentIn s m ec] [AgentOut s m ec]
process as parStrategy
    | parStrategy = runParSF asfs parCallback
    | otherwise = runSeqSF asfs seqCallback seqCallbackIteration
    where
        asfs = map adBehaviour as

----------------------------------------------------------------------------------------------------------------------
-- PARALLEL STRATEGY
----------------------------------------------------------------------------------------------------------------------
parCallback :: [AgentIn s m ec]
                -> [AgentOut s m ec]
                -> [AgentBehaviour s m ec]
                -> ([AgentBehaviour s m ec], [AgentIn s m ec])
parCallback oldAgentIns newAgentOuts asfs = (asfs', newAgentIns')
    where
        (asfs', newAgentIns) = processAgents asfs oldAgentIns newAgentOuts
        newAgentIns' = distributeMessages newAgentIns newAgentOuts

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
                handleAgent acc a@(sf, oldIn, newOut) = handleKillOrLiveAgent acc' a
                    where
                        acc' = handleCreateAgents acc newOut

                handleKillOrLiveAgent :: ([AgentBehaviour s m ec], [AgentIn s m ec])
                                            -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                            -> ([AgentBehaviour s m ec], [AgentIn s m ec])
                handleKillOrLiveAgent acc@(asfsAcc, ainsAcc) (sf, oldIn, newOut)
                    | kill = acc
                    | live = (asfsAcc ++ [sf], ainsAcc ++ [newIn])
                    where
                        kill = isEvent $ aoKill newOut
                        live = not kill
                        newIn = oldIn { aiStart = NoEvent,
                                        aiState = (aoState newOut),
                                        aiMessages = NoEvent }
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- SEQUENTIAL STRATEGY
----------------------------------------------------------------------------------------------------------------------
seqCallbackIteration :: [AgentOut s m ec] -> ([AgentBehaviour s m ec], [AgentIn s m ec])
seqCallbackIteration aouts = (newSfs, newSfsIns')
    where
        -- NOTE: messages of this agent are ALWAYS distributed, whether it is killed or not
        (newSfs, newSfsIns) = foldl handleCreateAgents ([], []) aouts
        -- NOTE: distribute messages to newly created agents as well
        newSfsIns' = distributeMessages newSfsIns aouts

-- NOTE: this callback feeds in all the inputs and the current working triple: SF, Inpout and Output
-- It allows to change the inputs of future SFs and may return the SF. if it doesnt return a SF this means it is deleted from the system
seqCallback :: [AgentIn s m ec] -- the existing inputs
                -> (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec) -- the current working triple
                -- optionally returns a sf-continuation for the current, can return new signal-functions and changed testinputs
                -> ([AgentIn s m ec],
                    Maybe (AgentBehaviour s m ec, AgentIn s m ec))
seqCallback allIns a@(sf, oldIn, newOut) = (allIns', maySfIn)
    where
        maySfIn = handleKillOrLiveAgent a
        -- NOTE: distribute messages to all other agents
        allIns' = distributeMessages allIns [newOut]

        handleKillOrLiveAgent :: (AgentBehaviour s m ec, AgentIn s m ec, AgentOut s m ec)
                                    -> Maybe (AgentBehaviour s m ec, AgentIn s m ec)
        handleKillOrLiveAgent (sf, oldIn, newOut)
            | kill = Nothing
            | live = Just (sf, newIn')
            where
                kill = isEvent $ aoKill newOut
                live = not kill
                newIn = oldIn { aiStart = NoEvent,
                                aiState = (aoState newOut),
                                aiMessages = NoEvent }
                -- NOTE: need to handle sending messages to itself because the input of this agent is not in the list of all inputs because it will be replaced anyway by newIn
                newIn' = collectMessagesFor [newOut] newIn
----------------------------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------------------------------------------
-- utils
----------------------------------------------------------------------------------------------------------------------
handleCreateAgents :: ([AgentBehaviour s m ec], [AgentIn s m ec])
                        -> AgentOut s m ec
                        -> ([AgentBehaviour s m ec], [AgentIn s m ec])
handleCreateAgents acc@(asfsAcc, ainsAcc) o
    | hasCreateAgents = (asfsAcc ++ newSfs, ainsAcc ++ newAis)
    | otherwise = acc
    where
        newAgentInheritedEnvironment = aoEnv o
        newAgentDefsEvt = aoCreate o
        hasCreateAgents = isEvent newAgentDefsEvt
        newAgentDefs = fromEvent newAgentDefsEvt
        newSfs = map adBehaviour newAgentDefs
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