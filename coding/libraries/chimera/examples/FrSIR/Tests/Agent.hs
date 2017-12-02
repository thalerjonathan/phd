module Agent 
    (
      test_agent_group
    ) where 

import System.Random (StdGen, mkStdGen)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.STM.TVar (newTVarIO, TVar)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import FRP.FrABS
import FRP.Yampa

import FrSIR.Model
import FrSIR.Agent

test_agent_group :: TestTree
test_agent_group =
    testGroup "gotInfected"
        []

-------------------------------------------------------------------------------
--test_gotInfected = QC.testProperty "gotInfected" $ monadic test_gotInfectedAux
--test_gotInfectedAux = monadic (gotInfected agentSusceptibleContactInfected)
-------------------------------------------------------------------------------

agentSusceptibleContactInfected :: FrSIRAgentIn
agentSusceptibleContactInfected = frSirAgentInMsgs 0 Susceptible [(0, Contact Infected)]

frSirAgentIn :: AgentId -> SIRState -> FrSIRAgentIn
frSirAgentIn aid s = frSirAgentInMsgs aid s []

frSirAgentInMsgs :: AgentId -> SIRState -> [AgentMessage FrSIRMsg] -> FrSIRAgentIn
frSirAgentInMsgs aid s msgs = AgentIn 
    { aiId = aid
    , aiMessages = msgsEvt
    , aiConversation = Nothing
    , aiStart = NoEvent
    , aiState = s
    , aiRec = NoEvent
    , aiRecInitAllowed = False
    , aiRng = dummyRng
    , aiIdGen = dummyIdGen
    }
  where
    msgsEvt = if null msgs then NoEvent else Event msgs 

dummyRng :: StdGen
dummyRng = mkStdGen 0

dummyIdGen :: TVar Int
dummyIdGen = unsafePerformIO  $ newTVarIO 0