{-# LANGUAGE Arrows #-}
module FrABS.Agent.Monad (
    agentIdM,
    environmentM,
    environmentPositionM,
    changeEnvironmentPositionM,

    createAgentM,
    killM,
    isDeadM,

    sendMessageM,
    sendMessagesM,
    broadcastMessageM,
    onMessageM,
    onMessageMState,

    conversationM,
    conversationEndM,
    conversationReplyMonadicRunner,
    conversationIgnoreReplyMonadicRunner,

    updateDomainStateM,
    getDomainStateM,
    setDomainStateM,
    domainStateFieldM,

    runEnvironmentM,

    agentMonadic,

    ifThenElse,
    ifThenElseM
  ) where

import FrABS.Agent.Agent
import FrABS.Env.Environment

import FRP.Yampa

import Control.Monad
import Control.Monad.Trans.State

------------------------------------------------------------------------------------------------------------------------
-- Monadic Agent Functions
------------------------------------------------------------------------------------------------------------------------
agentIdM :: State (AgentOut s m ec l) AgentId
agentIdM = state (\ao -> (aoId ao, ao))

environmentM :: State (AgentOut s m ec l) (Environment ec l)
environmentM = state (\ao -> (aoEnv ao, ao))

environmentPositionM :: State (AgentOut s m ec l) EnvCoord
environmentPositionM = state (\ao -> (aoEnvPos ao, ao))

changeEnvironmentPositionM :: EnvCoord -> State (AgentOut s m ec l) ()
changeEnvironmentPositionM pos = state (\ao -> ((), ao { aoEnvPos = pos }))


sendMessageM :: AgentMessage m -> State (AgentOut s m ec l) ()
sendMessageM msg = state (\ao -> ((), sendMessage ao msg))

conversationM :: AgentMessage m
                -> AgentConversationSender s m ec l
                -> State (AgentOut s m ec l) ()
conversationM msg replyHdl = state (\ao -> ((), conversation ao msg replyHdl))

conversationEndM :: State (AgentOut s m ec l) ()
conversationEndM = state (\ao -> ((), conversationEnd ao))

sendMessagesM :: [AgentMessage m] -> State (AgentOut s m ec l) ()
sendMessagesM msgs = state (\ao -> ((), sendMessages ao msgs))

broadcastMessageM :: m -> [AgentId] -> State (AgentOut s m ec l) ()
broadcastMessageM m receiverIds = state (broadcastMessageMAux m)
    where
        broadcastMessageMAux :: m -> AgentOut s m ec l -> ((), AgentOut s m ec l)
        broadcastMessageMAux m ao = ((), sendMessages ao msgs)
            where
                n = length receiverIds
                ms = replicate n m
                msgs = zip receiverIds ms

createAgentM :: AgentDef s m ec l -> State (AgentOut s m ec l) ()
createAgentM newDef = state (\ao -> ((),createAgent ao newDef))

conversationReplyMonadicRunner :: (Maybe (AgentMessage m) -> State (AgentOut s m ec l) ()) 
                                    -> AgentConversationSender s m ec l
conversationReplyMonadicRunner replyAction ao mayReply = execState (replyAction mayReply) ao

conversationIgnoreReplyMonadicRunner :: State (AgentOut s m ec l) () -> AgentConversationSender s m ec l
conversationIgnoreReplyMonadicRunner replyAction ao _ = execState replyAction ao

killM :: State (AgentOut s m ec l) ()
killM = state (\ao -> ((), ao { aoKill = Event () }))

isDeadM :: State (AgentOut s m ec l) Bool
isDeadM = state (\ao -> (isDead ao, ao))
   
onMessageMState :: AgentIn s m ec l -> (AgentMessage m -> State acc ()) -> State acc ()
onMessageMState ai msgHdl = onMessageM ai (\_ msg -> msgHdl msg) ()

onMessageM :: (Monad mon) => AgentIn s m ec l -> (acc -> AgentMessage m -> mon acc) -> acc -> mon acc
onMessageM ai msgHdl acc
    | not hasMessages = return acc
    -- | otherwise = foldM (\acc msg -> msgHdl acc msg) acc msgs
    | otherwise = foldM msgHdl acc msgs
    where
        msgsEvt = aiMessages ai
        hasMessages = isEvent msgsEvt
        msgs = fromEvent msgsEvt
    
updateDomainStateM :: (s -> s) -> State (AgentOut s m ec l) ()
updateDomainStateM sfunc = state (updateDomainStateMAux sfunc)
    where
        updateDomainStateMAux :: (s -> s) 
                            -> AgentOut s m ec l 
                            -> ((), AgentOut s m ec l)
        updateDomainStateMAux sfunc ao = ((), updateDomainState ao sfunc)

setDomainStateM :: s -> State (AgentOut s m ec l) ()
setDomainStateM s = state (\ao -> ((), setDomainState ao s))

domainStateFieldM :: (s -> t) -> State (AgentOut s m ec l) t
domainStateFieldM f = state (domainStateFieldMAux f)
    where
        domainStateFieldMAux :: (s -> t) 
                            -> AgentOut s m ec l
                            -> (t, AgentOut s m ec l)
        domainStateFieldMAux f ao = (f s, ao)
            where
                s = aoState ao

runEnvironmentM :: State (Environment ec l) a -> State (AgentOut s m ec l) a
runEnvironmentM envStateTrans =
    do
        env <- environmentM 
        let (a, env') = runState envStateTrans env
        setEnvironmentM env'
        return a

setEnvironmentM :: Environment ec l -> State (AgentOut s m ec l) ()
setEnvironmentM env =
    do
        ao <- get 
        put $ ao { aoEnv = env }

getDomainStateM :: State (AgentOut s m ec l) s
getDomainStateM = 
    do
        ao <- get
        let domainState = aoState ao 
        return domainState

agentMonadic :: (Double -> AgentIn s m ec l -> State (AgentOut s m ec l) ()) -> AgentBehaviour s m ec l
agentMonadic f = proc ain ->
    do
        age <- time -< 0

        let ao = agentOutFromIn ain
        let ao' = execState (f age ain) ao
        
        returnA -< ao'

------------------------------------------------------------------------------------------------------------------------
-- Monadic Utility Functions
------------------------------------------------------------------------------------------------------------------------
ifThenElse :: Monad m => Bool -> m a -> m a -> m a
ifThenElse p trueAction falseAction = if p then trueAction else falseAction

ifThenElseM :: Monad m => m Bool -> m a -> m a -> m a
ifThenElseM test trueAction falseAction = test >>= \t -> if t then trueAction else falseAction
------------------------------------------------------------------------------------------------------------------------