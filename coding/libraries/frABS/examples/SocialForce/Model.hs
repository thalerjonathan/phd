module SocialForce.Model (
      SocialForceMsg (..)
    , SocialForceEnvironment (..)
    , SocialForceAgentState (..)

    , PersonColor
    , PersonState (..)

    , SocialForceAgentDef
    , SocialForceAgentBehaviour
    , SocialForceAgentIn
    , SocialForceAgentOut
    , SocialForceAgentObservable

    , SocialForceAgentMonadicBehaviour
    , SocialForceAgentMonadicBehaviourReadEnv
    , SocialForceAgentMonadicBehaviourNoEnv

    , SocialForceSimulationParams

    , unitTime

    , enterSpeed
    , groupSpawningProb
    , pre_ppl_psy 
    , pre_range
    , pre_angle
    , pre_wall_psy

    , whiteColor
    , blackColor
    , randomColor
  ) where

import Control.Monad.Random

import FRP.FrABS
import FRP.Yampa

import SocialForce.Markup

data SocialForceMsg = SocialForceMsg

data SocialForceEnvironment = SocialForceEnvironment
  {
      sfEnvWalls :: [Wall]
  } deriving Show

type PersonColor = (Int, Int, Int)

data PersonState 
  = GoingToEntrance 
  | Moving
  | Reading
  | Waiting
  | Resting
  | Exiting
  | FindingDoor
  | Leaving deriving (Eq, Show)

data SocialForceAgentState = 
    Museum 
    {
        musStartPoint   :: Continuous2dCoord
      , musGroupPoint0  :: Continuous2dCoord
      , musGroupPoints  :: [Continuous2dCoord]
    }
  | Person
    {
        perState        :: PersonState
      , perPos          :: Continuous2dCoord
      , perSpeed        :: Continuous2dCoord
      , perHeading      :: Double

      , perArrivedDest  :: Bool
      , perDest         :: Continuous2dCoord

      , perColor        :: PersonColor

      , perVi0          :: Double
      , perAi           :: Double
      , perBi           :: Double
      , perK            :: Double
      , perk            :: Double
      , perRi           :: Double
      , perMi           :: Double
      
      , perConRange     :: Double
      , perAttRange     :: Double
      , perAiWall       :: Double
      , perAiGrp        :: Double
    
      , perBelGroup     :: Maybe AgentId
      , perDestScreen   :: AgentId

      , perSumFiWH      :: Double
      , perSumFiWV      :: Double
      , perSumFijH      :: Double
      , perSumFijV      :: Double

      , perInjured      :: Bool
    } deriving Show

type SocialForceAgentDef = AgentDef SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentBehaviour = AgentBehaviour SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentIn = AgentIn SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentOut = AgentOut SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentObservable = AgentObservable SocialForceAgentState

type SocialForceAgentMonadicBehaviour = AgentMonadicBehaviour SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentMonadicBehaviourReadEnv = AgentMonadicBehaviourReadEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment
type SocialForceAgentMonadicBehaviourNoEnv = AgentMonadicBehaviourNoEnv SocialForceAgentState SocialForceMsg SocialForceEnvironment

type SocialForceSimulationParams = SimulationParams SocialForceEnvironment

unitTime :: DTime
unitTime = 0.1

enterSpeed :: Double
enterSpeed = 2 -- TODO: re-set to 7

groupSpawningProb :: Double
groupSpawningProb = 0.3

pre_ppl_psy :: Double 
pre_ppl_psy = 2

pre_range :: Double 
pre_range = 10

pre_angle :: Double 
pre_angle = 5 * pi / 6;

pre_wall_psy :: Double
pre_wall_psy = 2

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