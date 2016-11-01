module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Debug.Trace

-------------------------------------------------------------------------------------------------------------
data MessageType d = AgentStart | AgentStop | AgentDomain d deriving (Show, Eq)

data Message d = Message
  {
    msgType :: MessageType d,
    msgContent :: Maybe String
  } deriving (Show)

data TestDomain = A | B | C deriving (Show, Eq)
type STMTestbedMsg = Message TestDomain

readMessages :: (Show d) => TChan (Message d) -> IO ()
readMessages box = do
  ret <- atomically $ tryReadTChan box
  case (ret) of Nothing -> return ()
                Just msg -> trace (show msg) (readMessages box)

createMsgBox :: IO (TChan (Message d))
createMsgBox = newTChanIO

main :: IO ()
main = do msgBox <- createMsgBox
          atomically $ writeTChan msgBox (Message { msgType = AgentStart, msgContent = Nothing })
          atomically $ writeTChan msgBox (Message { msgType = AgentStop, msgContent = Nothing })
          atomically $ writeTChan msgBox (Message { msgType = AgentDomain A, msgContent = Just "Domain Message A" })
          readMessages msgBox
-------------------------------------------------------------------------------------------------------------

type FuncMsg = (Int -> Int)

createFuncMsgBox :: IO (TChan FuncMsg)
createFuncMsgBox = newTChanIO

createMsgFunc :: Int -> (Int -> Int)
createMsgFunc n = (\x -> n*x)

readFuncMessages :: TChan FuncMsg -> IO ()
readFuncMessages box = do
  ret <- atomically $ tryReadTChan box
  case (ret) of Nothing -> return ()
                Just f -> trace (show (f 10)) (readFuncMessages box)
                
main' :: IO ()
main' = do msgBox <- createFuncMsgBox
           atomically $ writeTChan msgBox (createMsgFunc 1)
           atomically $ writeTChan msgBox (createMsgFunc 2)
           atomically $ writeTChan msgBox (createMsgFunc 3)
           readFuncMessages msgBox

-------------------------------------------------------------------------------------------------------------
