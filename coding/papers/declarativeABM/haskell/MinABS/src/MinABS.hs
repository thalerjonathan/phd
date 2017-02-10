module MinABS (
    Aid,
    Event(..),
    Agent(..),
    AgentTransformer,
    iterationSteps,
    iterationStart,
    iterationNext,
    send,
    broadcast,
    updateState,
    createAgent
  ) where

import Data.List

type Aid = Int

data Event m = Start | Dt Double | Message (Aid, m) deriving(Show)

data Agent s m = Agent {
    aid :: Aid,
    s :: s,
    m :: [(Aid, m)],
    f :: AgentTransformer s m
}

type AgentTransformer s m = (Agent s m -> Event m -> Agent s m )

instance (Show s, Show m) => Show (Agent s m) where
    show (Agent aid s m _) = "Agent " ++ (show aid) ++ ": state = " ++ (show s) ++ " mbox = " ++ (show m)

------------------------------------------------------------------------------------------------------------------------
-- PUBLIC
createAgent :: Aid -> s -> AgentTransformer s m -> Agent s m
createAgent id state trans = Agent { aid = id, s = state, m = [], f = trans }


iterationSteps :: [Agent s m] -> Int -> [Agent s m]
iterationSteps as n = stepMulti' as' n
    where
        as' = iterationStart as

        stepMulti' :: [Agent s m] -> Int -> [Agent s m]
        stepMulti' as 0 = as
        stepMulti' as n = stepMulti' as' (n-1)
            where
                as' = iterationNext as

iterationStart :: [Agent s m] -> [Agent s m]
iterationStart as = sendEvent as Start

iterationNext :: [Agent s m] -> [Agent s m]
iterationNext as = map (step as) as

send :: Agent s m -> (Aid, m) -> Agent s m
send a msg = a { m = (m a) ++ [msg]}

broadcast :: Agent s m -> ([Aid], m) -> Agent s m
broadcast a (rs, msg) = foldl (\a' rid -> send a' (rid, msg)) a rs

updateState :: Agent s m -> (s -> s) -> Agent s m
updateState a sf = a { s = s' }
    where
        s' = sf (s a)
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- PRIVATE
sendEvent :: [Agent s m] -> Event m -> [Agent s m]
sendEvent as e = map (\a -> (f a) a e ) as

step :: [Agent s m] -> Agent s m ->  Agent s m
step as a = advanceTime $ consumeMessages (a', msgs)
    where
        a' = a { m = [] }
        msgs = collectFor a as

collectFor :: Agent s m -> [Agent s m] -> [(Aid, m)]
collectFor a as = foldl (\acc a' -> acc ++ collectFrom a' (aid a)) [] as

collectFrom :: Agent s m -> Aid -> [(Aid, m)]
collectFrom a rid = map (\(_, m) -> ((aid a), m)) ms
    where
        ms = filter (\(rid', _) -> rid == rid') (m a)

consumeMessages :: (Agent s m, [(Aid, m)]) -> Agent s m
consumeMessages (a, ms) = foldl processMessage a ms
    where
        processMessage :: Agent s m -> (Aid, m) -> Agent s m
        processMessage a msg = (f a) a (Message msg)

advanceTime :: Agent s m -> Agent s m
advanceTime a = (f a) a (Dt 1.0)
------------------------------------------------------------------------------------------------------------------------
