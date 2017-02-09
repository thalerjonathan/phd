module MinABS where

import Data.List
import Control.Parallel.Strategies

type Aid = Int

data Event m = Start | Dt Double | Message (Aid, m) deriving(Show)

data Agent s m = Agent {
    aid :: Aid,
    s :: s,
    m :: [(Aid, m)],
    ns :: [Aid],
    f :: AgentTransformer s m
}

type AgentTransformer s m = (Agent s m -> Event m -> Agent s m )

instance (Show s, Show m) => Show (Agent s m) where
    show (Agent aid s m ns _) = "Agent " ++ (show aid) ++ ": state = " ++ (show s) ++ " mbox = " ++ (show m) ++ " neighbours: " ++ (show ns)

sendEvent :: [Agent s m] -> Event m -> [Agent s m]
sendEvent as e = map (\a -> (f a) a e ) as

send :: Agent s m -> (Aid, m) -> Agent s m
send a msg = a { m = (m a) ++ [msg]}

sendToNeighbours :: Agent s m -> m -> Agent s m
sendToNeighbours a msg = foldl (\a' n -> send a' (n, msg)) a (ns a)

updateState :: Agent s m -> (s -> s) -> Agent s m
updateState a sf = a { s = s' }
    where
        s' = sf (s a)

iterationSteps :: [Agent s m] -> Int -> [Agent s m]
iterationSteps as n = stepMulti' as' n
    where
        as' = startIteration as

        stepMulti' :: [Agent s m] -> Int -> [Agent s m]
        stepMulti' as 0 = as
        stepMulti' as n = stepMulti' as' (n-1)
            where
                as' = iteration as

startIteration :: [Agent s m] -> [Agent s m]
startIteration as = sendEvent as Start

iteration :: [Agent s m] -> [Agent s m]
iteration = stepAll . collectAll

collectAll :: [Agent s m] -> [(Agent s m, [(Aid, m)])]
collectAll as = map (\a -> (a, collectFor a as)) as

collectFor :: Agent s m -> [Agent s m] -> [(Aid, m)]
collectFor a as = foldl (\acc a' -> acc ++ collectFrom a' (aid a)) [] as

collectFrom :: Agent s m -> Aid -> [(Aid, m)]
collectFrom a rid = map (\(_, m) -> ((aid a), m)) ms
    where
        ms = filter (\(rid', _) -> rid == rid') (m a)

stepAll :: [(Agent s m, [(Aid, m)])] -> [Agent s m]
stepAll = map step

step :: (Agent s m, [(Aid, m)]) -> Agent s m
step (a, msgs) = advanceTime $ consumeMessages (a', msgs)
    where
        a' = a { m = [] }

consumeMessages :: (Agent s m, [(Aid, m)]) -> Agent s m
consumeMessages (a, ms) = foldl processMessage a ms
    where
        processMessage :: Agent s m -> (Aid, m) -> Agent s m
        processMessage a msg = (f a) a (Message msg)

advanceTime :: Agent s m -> Agent s m
advanceTime a = (f a) a (Dt 1.0)

createAgent :: Aid -> s -> AgentTransformer s m -> Agent s m
createAgent id state trans = Agent { aid = id, s = state, m = [], ns = [], f = trans }

addNeighbour :: Agent s m -> Agent s m -> Agent s m
addNeighbour a n = a { ns = (aid n) : (ns a)}