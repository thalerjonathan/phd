module PureAgentsSTM (
    module Control.Monad.STM,
    module Control.Concurrent.STM.TChan,
    module Control.Concurrent.STM.TVar,
    module Data.Maybe,
    Agent(..),
    SimHandle(..),
    AgentId,
    MsgHandler,
    UpdateHandler,
    kill,
    newAgent,
    sendMsg,
    sendMsgToRandomNeighbour,
    broadcastMsg,
    updateState,
    addNeighbours,
    stepSimulation,
    initStepSimulation,
    advanceSimulation,
    runSimulation,
    createAgent,
    changeEnv,
    readEnv,
    writeEnv,
    extractEnv,
    extractAgents
  ) where

import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Data.Maybe

import System.Random

import qualified Data.HashMap as Map

{-|
    This is an attempt on implementing a global-decision semantics ABM/S using STM but not running in parallel!
    For running in parallel we would need to carefully redesign the whole atomically stuff and in the end run in the IO-Monad, something we don't want here

    AGAIN: THIS IS NOT INTENDED TO RUN IN PARALLEL AS IT WILL NOT WORK CORRECTLY (OR AT ALL) WHEN EXECUTING CONCURRENTLY!
    AGAIN: THIS IS NOT INTENDED TO RUN IN PARALLEL AS IT WILL NOT WORK CORRECTLY (OR AT ALL) WHEN EXECUTING CONCURRENTLY!
    AGAIN: THIS IS NOT INTENDED TO RUN IN PARALLEL AS IT WILL NOT WORK CORRECTLY (OR AT ALL) WHEN EXECUTING CONCURRENTLY!
    AGAIN: THIS IS NOT INTENDED TO RUN IN PARALLEL AS IT WILL NOT WORK CORRECTLY (OR AT ALL) WHEN EXECUTING CONCURRENTLY!
    AGAIN: THIS IS NOT INTENDED TO RUN IN PARALLEL AS IT WILL NOT WORK CORRECTLY (OR AT ALL) WHEN EXECUTING CONCURRENTLY!

    NOTE: almost all functions want an Agent and return an Agent: this is how we communicate changes to the simulation-system: by changing the agent
-}

-- TODO: Yampa/Dunai
    -- TODO: let the whole thing run in Yampa/Dunai so we can leverage the power of the EDSL, SFs, continuations,... of Yampa/Dunai. But because running in STM must use Dunai
    -- TODO: implement wait blocking for a message so far. utilize yampas event mechanism?

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC, exported
------------------------------------------------------------------------------------------------------------------------
type AgentId = Int
type MsgHandler m s e = (Agent m s e -> m -> AgentId -> STM (Agent m s e))          -- NOTE: need STM to be able to send messages
type UpdateHandler m s e = (Agent m s e -> Double -> STM (Agent m s e))             -- NOTE: need STM to be able to send messages
type OutFunc m s e = (([Agent m s e], Maybe e) -> IO (Bool, Double))

{- NOTE:    m is the type of messages the agent understands
            s is the type of a generic state of the agent e.g. any data
            e is the type of a generic environment the agent can act upon
-}
data Agent m s e = Agent {
    agentId :: AgentId,
    killFlag :: Bool,
    mbox :: (TChan (AgentId, m)),
    neighbourIds :: [AgentId],
    neighbourMbox :: Map.Map AgentId (TChan (AgentId, m)),        -- NOTE: strength of haskell: ensure by static typing that only neighbours with same message-protocoll
    msgHandler :: MsgHandler m s e,
    updateHandler :: UpdateHandler m s e,
    state :: s,
    newAgents :: [Agent m s e],
    env :: Maybe (TVar e)                                      -- NOTE: environment is optional
}

data SimHandle m s e = SimHandle {
    simHdlAgents :: [Agent m s e],
    simHdlEnv :: Maybe (TVar e)
}

{-|
    The 'newAgent' function takes a parent Agent and a new Agent to be added to the Simulation
    It will be running on the next global-step of the Simulation.

    Returns the parent Agent
-}
newAgent :: Agent m s e -> Agent m s e -> Agent m s e
newAgent aParent aNew = aParent { newAgents = nas ++ [aNew'] }
    where
        aNew' = aNew { env = (env aParent)} -- NOTE: set to same environment
        nas = newAgents aParent

{-|
    Marks an Agent as to be killed at the end of the global-step of the simulation
-}
kill :: Agent m s e -> Agent m s e
kill a = a { killFlag = True }

{-|
    SEnds a message to the target. Note that the message will be seen immediately by the target which could be in
    the same global-step of the simulation if the target-Agent runs AFTER the message has been sent (either because of parallel- or sequential-update)
-}
sendMsg :: Agent m s e -> m -> AgentId -> STM ()
sendMsg a m targetId
    | isNothing targetMbox = return ()                          -- NOTE: receiver not found in the neighbours
    | otherwise = writeTChan (fromJust targetMbox) (senderId, m)        -- NOTE: we commit the message immediately
    where
        nsBox = neighbourMbox a
        senderId = agentId a
        targetMbox = Map.lookup targetId nsBox

broadcastMsg :: Agent m s e -> m -> STM ()
broadcastMsg a msg = do
                        actions <- mapM (\mbox -> writeTChan mbox (senderId, msg)) nsBox
                        return ()

    where
        nsBox = neighbourMbox a
        senderId = agentId a


sendMsgToRandomNeighbour :: (RandomGen g) => Agent m s e -> m -> g -> STM g
sendMsgToRandomNeighbour a msg g = do
                                    sendMsg a msg randAgentId
                                    return g'
                                        where
                                            s = state a
                                            nIds = neighbourIds a
                                            nsCount = length nIds
                                            (randIdx, g') = randomR(0, nsCount-1) g
                                            randAgentId = nIds !! randIdx

updateState :: Agent m s e -> (s -> s) -> Agent m s e
updateState a sf = a { state = s' }
    where
        s = state a
        s' = sf s

createAgent :: AgentId -> s -> MsgHandler m s e -> UpdateHandler m s e -> STM (Agent m s e)
createAgent i s mhdl uhdl = do
                                mb <- newTChan
                                return Agent{ agentId = i,
                                                state = s,
                                                mbox = mb,
                                                killFlag = False,
                                                neighbourMbox = Map.empty,
                                                neighbourIds = [],
                                                newAgents = [],
                                                msgHandler = mhdl,
                                                updateHandler = uhdl,
                                                env = Nothing }

addNeighbours :: Agent m s e -> [Agent m s e] -> Agent m s e
addNeighbours a ns = a { neighbourMbox = nsMbox', neighbourIds = nsIds' }
    where
        nsMbox = neighbourMbox a
        nsMbox' = foldl (\acc a -> Map.insert (agentId a) (mbox a) acc ) nsMbox ns
        nsIds = neighbourIds a
        nsIds' = nsIds ++ (map agentId ns)

changeEnv :: Agent m s e -> (e -> e) -> STM ()
changeEnv a tx = do
                    let maybeEnv = env a
                    if (isNothing maybeEnv) then
                        return ()                               -- NOTE: no environment, do nothing
                        else
                            modifyTVar (fromJust maybeEnv) tx

readEnv :: Agent m s e -> STM e
readEnv a = readTVar (fromJust (env a))

writeEnv :: Agent m s e -> e -> STM ()
writeEnv a e = changeEnv a (\_ -> e)

extractEnv :: SimHandle m s e -> STM (Maybe e)
extractEnv hdl = do
                     let mayEnvVar = simHdlEnv hdl
                     env <- maybeEnvVarToMaybeEnv mayEnvVar
                     return env

extractAgents :: SimHandle m s e -> [Agent m s e]
extractAgents = simHdlAgents

-- TODO: return all steps of agents and environment
stepSimulation :: [Agent m s e] -> Maybe e -> Double -> Int -> STM ([Agent m s e], Maybe e)
stepSimulation as e dt n = do
                                (asWithEnv, mayEnvVar) <- setEnv as e
                                asFinal <- stepSimulation' asWithEnv dt n
                                finalEnv <- maybeEnvVarToMaybeEnv mayEnvVar
                                return (asFinal, finalEnv)
    where
        stepSimulation' :: [Agent m s e] -> Double -> Int -> STM [Agent m s e]
        stepSimulation' as dt 0 = return as
        stepSimulation' as dt n = do
                                    as' <- stepAllAgents as dt
                                    stepSimulation' as' dt (n-1)

initStepSimulation :: [Agent m s e] -> Maybe e -> STM ([Agent m s e], SimHandle m s e)
initStepSimulation as e = do
                            (asWithEnv, mayEnvVar) <- setEnv as e
                            let hdl = SimHandle { simHdlAgents = asWithEnv, simHdlEnv = mayEnvVar }
                            return (asWithEnv, hdl)

advanceSimulation :: SimHandle m s e -> Double -> STM ([Agent m s e], Maybe e, SimHandle m s e)
advanceSimulation hdl dt = do
                                let as = simHdlAgents hdl
                                as' <- stepAllAgents as dt
                                env' <- extractEnv hdl
                                let hdl' = hdl { simHdlAgents = as' }         -- NOTE: environment never changes after initial
                                return (as', env', hdl')

runSimulation :: [Agent m s e] -> Maybe e -> OutFunc m s e -> IO ()
runSimulation as e out = do
                            (asWithEnv, mayEnvVar) <- atomically $ setEnv as e
                            runSimulation' asWithEnv 0.0 mayEnvVar out

        where
            runSimulation' :: [Agent m s e] -> Double -> Maybe (TVar e) -> OutFunc m s e -> IO ()
            runSimulation' as dt mayEnvVar out = do
                                        as' <- atomically $ stepAllAgents as dt
                                        finalEnv <- atomically $ maybeEnvVarToMaybeEnv mayEnvVar
                                        (cont, dt') <- out (as', finalEnv)
                                        if cont == True then
                                            runSimulation' as' dt' mayEnvVar out
                                            else
                                                return ()

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE, non exports
------------------------------------------------------------------------------------------------------------------------
stepAllAgents :: [Agent m s e] -> Double -> STM [Agent m s e]
stepAllAgents as dt = foldl (\acc a -> do
                                        aAfterStep <- stepAgent dt a
                                        acc' <- acc
                                        if ( killFlag aAfterStep ) then   -- NOTE: won't notify other agents about the death, this can be implemented by domain-specific messages if required
                                            return (acc' ++ (newAgents aAfterStep)) -- NOTE: no need to clear the list because this agent will be deleted anyway
                                            else
                                                do
                                                    let nas = newAgents aAfterStep
                                                    let a' = aAfterStep { newAgents = [] }
                                                    return (acc' ++ [a'] ++ nas)
                                        ) (return []) as

maybeEnvVarToMaybeEnv :: Maybe (TVar e) -> STM (Maybe e)
maybeEnvVarToMaybeEnv Nothing = return Nothing
maybeEnvVarToMaybeEnv (Just var) = do
                                    e <- readTVar var
                                    return (Just e)

setEnv :: [Agent m s e] -> Maybe e -> STM ([Agent m s e], Maybe (TVar e))
setEnv as Nothing = return (as, Nothing)
setEnv as (Just env) = do
                        envVar <- newTVar env
                        let as' = Prelude.map (\a -> a { env = Just envVar } ) as
                        return (as', Just envVar)


receiveMsg :: Agent m s e -> STM (Maybe (AgentId, m))
receiveMsg a = tryReadTChan mb
    where
        mb = mbox a

processMsg :: Agent m s e -> (AgentId, m) -> STM (Agent m s e)
processMsg a (senderId, msg) = handler a msg senderId
    where
        handler = msgHandler a

processAllMessages :: Agent m s e -> STM (Agent m s e)
processAllMessages a = do
                        msg <- receiveMsg a
                        if ( isNothing msg ) then
                            return a
                                else
                                    do
                                        a' <- processMsg a (fromJust msg)
                                        processAllMessages a'

stepAgent :: Double -> Agent m s e -> STM (Agent m s e)
stepAgent dt a = do
                    aAfterMsgProc <- processAllMessages a
                    let upHdl = updateHandler aAfterMsgProc             -- NOTE: updateHandler could have changed!
                    aAfterUpdt <- upHdl aAfterMsgProc dt
                    return aAfterUpdt
------------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------------
{- TODO: experiment with 'active' messages which represent a conversation between two agents but will be executed only by
         the receiving agent and will result in a final result which is then local to the receiving agent. This allows
          to encapsulate data in a more general way through closures and to have some logic working upon the data. -}
{-
main :: IO ()
main = do
    let b = makeOffer bid
    let r = executeOffer ask b
    putStrLn (show r)
        where
            bid = Bid 42
            ask = Ask 41

data Offering a = Bid a | Ask a | Accept | Refuse deriving (Show)

data Conversation m = Msg m | Conv (m -> Conversation m)

{- NOTE: this part sends an offer enclosed in the lambda and will compare it to a passed in offer where a reply is then made
         TODO: introduce additional step> send o along with an additional reply and wait
-}
makeOffer :: Offering Int -> Conversation (Offering Int)
makeOffer o = Conv (\o' -> if compareOffer o o' then
                                Msg Accept
                                    else
                                        Msg Refuse )

{- NOTE: this part is the counterpart which allows to execute a given offering-conversation with a given offering
         problem: we loose track were we are like in makeOffer
         TODO: must always terminate and finally return Message

-}
executeOffer :: Offering Int -> Conversation (Offering Int) -> Offering Int
executeOffer o (Conv f) = executeOffer o (f o)
executeOffer o (Msg o') = o'

compareOffer :: Offering Int -> Offering Int -> Bool
compareOffer (Bid a) (Ask a') = a >= a'
compareOffer (Ask a) (Bid a') = a <= a'
compareOffer _ _ = False
-}
-----------------------------------------------------------------------------------------------------------------------------
