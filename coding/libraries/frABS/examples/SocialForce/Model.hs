module SocialForce.Model (
      SocialForceMsg (..)
    , SocialForceEnvironment (..)
    , SocialForceAgentState (..)

    , PersonColor
    , PersonState (..)

    , GroupId
    , Group (..)

    , PersonEnvObs (..)
    
    , SocialForceAgentDef
    , SocialForceAgentBehaviour
    , SocialForceAgentIn
    , SocialForceAgentOut
    , SocialForceAgentObservable

    , SocialForceAgentMonadicBehaviour
    , SocialForceAgentMonadicBehaviourReadEnv
    , SocialForceAgentMonadicBehaviourNoEnv

    , SocialForceReactiveBehaviourIgnoreEnv
    , SocialForceReactiveBehaviourReadEnv

    , SocialForceEventSource
    , SocialForceSimulationParams

    , isPerson
    , unitTime

    , enterSpeed
    , groupSpawningProb
    , vi0Init
    , enableVisionArea

    , bottomStartPoints
    , topStartPoints
    , topEntrance
    , bottomEntrance
    , topExit
    , bottomExit

    , entrance
    , exit
    , startPoints

    , whiteColor
    , blackColor
    , randomColor
  ) where

import Control.Monad.Random
import qualified Data.Map.Strict as Map

import FRP.FrABS
import FRP.Yampa

import SocialForce.Markup

data SocialForceMsg = SocialForceMsg

data SocialForceEnvironment = SocialForceEnvironment
  {
      sfEnvWalls        :: [Line]
    --, sfEnvTopEntr      :: Continuous2dCoord
    --, sfEnvTopExit      :: Continuous2dCoord
    --, sfEnvBotEntr      :: Continuous2dCoord
    --, sfEnvBotExit      :: Continuous2dCoord
    --, sfEnvTopStart     :: [Continuous2dCoord]
    --, sfEnvBotStart     :: [Continuous2dCoord]
    
    , sfEnvMovingArea   :: Rect
    , sfEnvPeos         :: Map.Map AgentId PersonEnvObs
  } deriving Show

type GroupId = AgentId

data Group = Group
  {
      grpId           :: GroupId
    , grpDest         :: Continuous2dCoord
    , grpReadingTime  :: Double
    , grpModified     :: Bool
    , grpExit         :: Bool
    , grpColor        :: PersonColor
    , grpPersons      :: [AgentId]
  } deriving Show

type PersonColor = (Int, Int, Int)

data PersonState 
  = GoingToEntrance 
  | Moving
  | Holding
  | FindingDoor
  | Exiting
  | Leaving 
  | Left deriving (Eq, Show)

data PersonEnvObs = PersonEnvObs
  {
      peoPos    :: Continuous2dCoord
    , peoSpeed  :: Continuous2dCoord
    , peoRi     :: Double
    , peoGroup  :: Maybe AgentId
  } deriving Show

data SocialForceAgentState = 
    Hall 
    {
      hallGroups :: [Group]
    }
  | Person
    {
        perState        :: PersonState
      , perTop          :: Bool  

      , perPos          :: Continuous2dCoord
      , perDest         :: Continuous2dCoord
      , perSpeed        :: Continuous2dCoord
      , perHeading      :: Double

      , perEntry        :: Continuous2dCoord

      , perAttAngle     :: Double
      , perConRange     :: Double

      , perColor        :: PersonColor

      , perGroup        :: Maybe AgentId

      , perReadingTime  :: Double

      , perInjured      :: Bool

      , perArrivedDest  :: Bool

      , perAiWall       :: Double
      , perAiGrp        :: Double
      , perBi           :: Double
      , perAi           :: Double
      , perK            :: Double
      , perk            :: Double
      , perRi           :: Double
      , perVi0          :: Double
      , perMi           :: Double

      , perApplyPsy     :: Bool

      , perSumFiWH      :: Double
      , perSumFiWV      :: Double
      , perSumFijH      :: Double
      , perSumFijV      :: Double
    } deriving Show

type SocialForceAgentDef = AgentDef SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentBehaviour = AgentBehaviour SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentIn = AgentIn SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentOut = AgentOut SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentObservable = AgentObservable SocialForceAgentState

type SocialForceAgentMonadicBehaviour = AgentMonadicBehaviour SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentMonadicBehaviourReadEnv = AgentMonadicBehaviourReadEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentMonadicBehaviourNoEnv = AgentMonadicBehaviourNoEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment

type SocialForceReactiveBehaviourIgnoreEnv = ReactiveBehaviourIgnoreEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment 
type SocialForceReactiveBehaviourReadEnv = ReactiveBehaviourReadEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment 

type SocialForceEventSource = EventSource SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceSimulationParams = SimulationParams SocialForceEnvironment

isPerson :: SocialForceAgentState -> Bool
isPerson (Person {}) = True
isPerson _ = False

unitTime :: DTime
unitTime = 0.1

enterSpeed :: Double
enterSpeed = 2 -- TODO: re-set to 7

groupSpawningProb :: Double
groupSpawningProb = 0.3

vi0Init :: Double
vi0Init = 1.4

enableVisionArea :: Bool
enableVisionArea = False

bottomStartPoints :: [Continuous2dCoord]
bottomStartPoints = [(11.2, 19.6), (11.6, 20.8), (12.4, 19.6), (13.2, 20.8), (13.6, 19.6)]

topStartPoints :: [Continuous2dCoord]
topStartPoints = [(11.2, 2.4), (11.6, 1.2), (12.4, 2.4), (13.2, 1.2), (13.6, 2.4)]

topEntrance :: Continuous2dCoord
topEntrance = (12.4, 5.6)

bottomEntrance :: Continuous2dCoord
bottomEntrance = (12.4, 16.8)

topExit :: Continuous2dCoord
topExit = (3.6, 4.4)

bottomExit :: Continuous2dCoord
bottomExit = (3.6, 17.6)

blackColor :: PersonColor
blackColor = (0, 0, 0)

whiteColor :: PersonColor
whiteColor = (255, 255, 255)

randomColor :: Rand StdGen PersonColor
randomColor = do
  r <- getRandomR (0, 255)
  g <- getRandomR (0, 255)
  b <- getRandomR (0, 255)
  return (r, g, b)

entrance :: Bool -> Continuous2dCoord
entrance True = topEntrance
entrance _ = bottomEntrance

exit :: Bool -> Continuous2dCoord
exit True = topExit
exit _ = bottomExit

startPoints :: Bool -> [Continuous2dCoord]
startPoints True = topStartPoints
startPoints _ = bottomStartPoints