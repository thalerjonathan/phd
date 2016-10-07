module Main where
import System.IO
import Control.Concurrent.Actor

main :: IO ()
main = do
  ping <- spawn pingPongActor
  say $ "Main thread created ping-actor: " ++ show ping
  pong <- spawn pingPongActor
  say $ "Main thread created pong-actor: " ++ show pong
  r <- send ping "Ping"
  r <- send pong "Pong"
  return ()
  
pingPongActor :: MBox String -> IO a
pingPongActor mbox =
  receive mbox (\m -> say $ m )

{-
pingPongMsgReceived :: String -> IO a
pingPongMsgReceived "Ping" = say $ "received Ping" 
pingPongMsgReceived "Pong" = say $ "received Pong"
-}

{- TODO agent-model
- need a mechanism to encapsulate state of an actor and change this state upon reception of a message
- need a mechanism to receive / send generic messages: define protocoll
- need a mechanism to "connect" two actors: make the id of the actors known to each other through a specific message
-}


{- TODO:
- pattern match on Agent s: sender
- pattern match on Message m: message

data MessageType = AgentConnection |

1st argument: sender of message
2nd argument: message
3rd argument: receiver of message
return argument: receiver of message (for change)
agentReceivedMessage :: Agent s -> Message m -> Agent r -> Agent r
-}

------------------------------------------------------------------------------------------------------------
-- HACTORS Library Examples

dolphin :: String -> IO ()
dolphin "do a flip" = say "How about no?"
dolphin "fish"      = say "So long and thanks for all the fish!"
dolphin _           = say "Heh, we're smarter than you humans."

test_dolphin :: IO ()
test_dolphin = do
  hSetBuffering stdout LineBuffering
  dol <- spawn_receive dolphin
  dol ! "do a flip"
  dol ! "fish"
  dol ! "oh, hello dolphin!"
  return ()

spawn_1 :: IO ()
spawn_1 = do
  hSetBuffering stdout LineBuffering
  child <- spawn $ const $ say $ "  Hi, I'am the child."
  say $ "I was create a child with PID = " ++ show child

-- | Make an initial argument type.
data Child = Child String Process

-- | Using @actor@.
actor_1 :: IO ()
actor_1 = do
  hSetBuffering stdout LineBuffering
  me <- self
  say $ "Hi, I'am the parrent."
  child <- actor (Child "child" me) $
    \(Child name parent) _ -> do
      say $ "  Hi, I'am the one who was called " ++ name
      say $ "  My parrent's PID = " ++ show parent
  say $ "I was create a child with PID = " ++ show child
------------------------------------------------------------------------------------------------------------
