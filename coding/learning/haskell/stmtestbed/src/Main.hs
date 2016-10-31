module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

data MessageType d = AgentStart | AgentStop | AgentDomain d deriving (Show, Eq)

data Message d = Message
  {
    msgType :: MessageType d,
    msgContent :: Maybe String
  } deriving (Show)

data TestDomain = A | B | C deriving (Show, Eq)
type STMTestbedMsg = Message TestDomain

putMessage :: Message d -> TChan (Message d) -> STM ()
putMessage msg box = writeTChan box msg

readMessages :: (Show d) => TChan (Message d) -> STM ()
readMessages box = do
  ret <- tryReadTChan box
  case (ret) of Nothing -> return ()
                Just msg -> trace (show msg) (readMessages box)

testSTM :: Int -> STM ()
testSTM n = do msgBox <- newTChan
               writeTChan msgBox (Message { msgType = AgentStart, msgContent = Nothing })
               writeTChan msgBox (Message { msgType = AgentStop, msgContent = Nothing })
               writeTChan msgBox (Message { msgType = AgentDomain A, msgContent = Just "Domain Message A" })
               readMessages msgBox
          
          
