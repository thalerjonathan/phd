module Main where

import qualified Data.Map as Map
import Data.Maybe

type AgentId = Int

data AgentDef s m =
    OnAnyMessage (AgentDef s m) (AgentDef s m)
    | SendMessage (AgentId, m) (AgentDef s m)
    | UpdateState (s -> s) (AgentDef s m)
    | RepeatWith (AgentDef s m)
    | Repeat
    | DoNothing

interpretAgent :: s 
                    -> [(AgentId, m)] 
                    -> AgentDef s m 
                    -> AgentDef s m 
                    -> (s, [(AgentId, m)], AgentDef s m)
interpretAgent s ms ad' (OnAnyMessage t f) 
    | not . null $ ms = interpretAgent s ms ad' t
    | otherwise = interpretAgent s ms ad' f

interpretAgent s ms ad' (SendMessage msg ad) = interpretAgent s ms' ad' ad
    where
        ms' = msg : ms

interpretAgent s ms ad' (UpdateState f ad) = interpretAgent s' ms ad' ad
    where
        s' = f s

interpretAgent s ms ad' (RepeatWith ad) = (s, ms, ad)

interpretAgent s ms ad' Repeat = (s, ms, ad')

interpretAgent s ms ad' DoNothing = (s, ms, DoNothing)

main :: IO ()
main = 
    do
        {--
        let ad0 = OnAnyMessage
                    (SendMessage (1, ()) (UpdateState (\s -> succ 1) Repeat)) 
                    (SendMessage (1, ()) Repeat)
        let ad1 = OnAnyMessage 
                    (SendMessage (0, ()) (UpdateState (\s -> pred 1) Repeat)) 
                    (SendMessage (0, ()) Repeat)
        --}

        let ad0 = UpdateState (\s -> s + 1) Repeat
        let ad1 = UpdateState (\s -> s - 1) Repeat

        let a0 = (0, 42, ad0)
        let a1 = (1, (-42), ad1)

        let as = [a0, a1]
        let as' = runAgents 5000 as

        mapM_ (\(_, s, _) -> print s) (head as')
        putStrLn ""
        mapM_ (\(_, s, _) -> print s) (last as')

runAgents :: Int -> [(AgentId, s, AgentDef s m)] -> [[(AgentId, s, AgentDef s m)]]
runAgents n as = ass
    where
        (_, ass) = iterateAgents n as Map.empty

        iterateAgents :: Int ->
                            [(AgentId, s, AgentDef s m)] -> 
                            (Map.Map AgentId [(AgentId, m)]) ->
                            (Map.Map AgentId [(AgentId, m)], [[(AgentId, s, AgentDef s m)]])
        iterateAgents n as msgMap 
            | n <= 0 = (msgMap, [as])
            | otherwise = (msgMap'', as' : ass)
            where
                (msgMap', as') = foldr iterateAgentsAux (msgMap, []) as
                (msgMap'', ass) = iterateAgents (n - 1) as' msgMap'

                iterateAgentsAux :: (AgentId, s, AgentDef s m) -> 
                                        (Map.Map AgentId [(AgentId, m)], [(AgentId, s, AgentDef s m)]) ->
                                        (Map.Map AgentId [(AgentId, m)], [(AgentId, s, AgentDef s m)]) 
                iterateAgentsAux (aid, s, ad) (accMsgs, accAs) = (accMsgs'', accAs')
                    where
                        mayRecvMsgs = Map.lookup aid accMsgs
                        recvMsgs = maybe [] id mayRecvMsgs

                        (s', sendMsgs, ad') = interpretAgent s recvMsgs ad ad

                        accMsgs' = Map.delete aid accMsgs
                        accMsgs'' = distributeMessages aid sendMsgs accMsgs'
                        accAs' = (aid, s', ad') : accAs

distributeMessages :: AgentId 
                        -> [(AgentId, m)] 
                        -> Map.Map AgentId [(AgentId, m)]
                        -> Map.Map AgentId [(AgentId, m)]
distributeMessages senderId msgs msgMap = foldr distributeMessagesAux msgMap msgs
    where
        distributeMessagesAux :: (AgentId, m) 
                                    -> Map.Map AgentId [(AgentId, m)] 
                                    -> Map.Map AgentId [(AgentId, m)]
        distributeMessagesAux (receiverId, m) accMsgs = accMsgs'
            where
                mayReceiverMsgs = Map.lookup receiverId accMsgs
                receiverMsgs = maybe [] id mayReceiverMsgs
                receiverMsgs' = (senderId, m) : receiverMsgs 

                accMsgs' = Map.insert receiverId receiverMsgs' accMsgs