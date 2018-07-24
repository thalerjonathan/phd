module Model 
  (
    AgentId

  , SugAgentState (..)
  , SugAgentObservable (..)

  , SugEnvCellOccupier (..)
  , SugEnvCell (..)

  , SugEnvironment

  , SugContext (..)
  , SugAgent
  , SugAgentMonad

  , SugAgentIn (..)
  , SugAgentOut (..)

  , nextAgentId
  , readEnvironment
  , writeEnvironment
  , (<°>)
  
  , sugarGrowbackUnits
  , sugarCapacityRange
  , sugarEndowmentRange
  , sugarEndowmentRangeStandard
  , sugarMetabolismRange
  , visionRange
  , visionRangeStandard
  ) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Concurrent.STM
import FRP.BearRiver

import Discrete

------------------------------------------------------------------------------------------------------------------------
-- DOMAIN-SPECIFIC AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
type AgentId = Int

data SugAgentState = SugAgentState 
  { sugAgCoord            :: Discrete2dCoord
  , sugAgSugarMetab       :: Double              -- this amount of sugar will be consumed by the agent in each time-step
  , sugAgVision           :: Int                 -- the vision of the agent: strongly depends on the type of the environment: Int because its 2d discrete
  , sugAgSugarLevel       :: Double              -- the current sugar holdings of the agent, if 0 then the agent starves to death
  , sugAgSugarInit        :: Double              -- agent is fertile only when its sugarlevel is GE than its initial endowment
  } deriving (Show)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord    :: Discrete2dCoord
  , sugObsVision   :: Int
  } deriving (Show)

data SugEnvCellOccupier = SugEnvCellOccupier 
  { sugEnvOccId     :: AgentId
  , sugEnvOccWealth :: Double
  } deriving (Show)

data SugEnvCell = SugEnvCell 
  { sugEnvSugarCapacity :: Double
  , sugEnvSugarLevel    :: Double
  , sugEnvOccupier      :: Maybe SugEnvCellOccupier
  } deriving (Show)

type SugEnvironment = Discrete2d SugEnvCell

data SugAgentIn = SugAgentIn 

data SugAgentOut g = SugAgentOut 
  { sugAoKill       :: !(Event ())
  , sugAoNew        :: ![(AgentId, SugAgent g)]
  , sugAoObservable :: !(Maybe SugAgentObservable)
  }

data SugContext = SugContext 
  { sugCtxEnv     :: TVar SugEnvironment
  , sugCtxNextAid :: TVar AgentId
  }

type SugAgentMonad g = ReaderT SugContext (RandT g STM)
type SugAgent g      = SF (SugAgentMonad g) SugAgentIn (SugAgentOut g)

nextAgentId :: RandomGen g
            => (SugAgentMonad g) AgentId
nextAgentId = do
  ctx <- ask
  let aidVar = sugCtxNextAid ctx

  aid <- lift $ lift $ readTVar aidVar
  _   <- lift $ lift $ writeTVar aidVar (aid + 1)

  return aid
  
readEnvironment :: RandomGen g 
                => (SugAgentMonad g) SugEnvironment
readEnvironment = do
  ctx <- ask
  let envVar = sugCtxEnv ctx
  lift $ lift $ readTVar envVar

writeEnvironment :: RandomGen g 
                => SugEnvironment
                -> (SugAgentMonad g) ()
writeEnvironment e = do
  ctx <- ask
  let envVar = sugCtxEnv ctx
  lift $ lift $ writeTVar envVar e

(<°>) :: SugAgentOut g
      -> SugAgentOut g
      -> SugAgentOut g
(<°>) ao1 ao2 = SugAgentOut 
    { sugAoKill       = mergeBy (\_ _ -> ()) (sugAoKill ao1) (sugAoKill ao2)
    , sugAoNew        = sugAoNew ao1 ++ sugAoNew ao2
    , sugAoObservable = decideMaybe (sugAoObservable ao1) (sugAoObservable ao2)
    }
  where
    decideMaybe :: Maybe a 
                -> Maybe a 
                -> Maybe a
    decideMaybe Nothing Nothing  = Nothing
    decideMaybe (Just a) Nothing = Just a
    decideMaybe Nothing (Just a) = Just a
    decideMaybe _       _        = error "Can't decide between two Maybes"
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- CHAPTER II: Life And Death On The Sugarscape
------------------------------------------------------------------------------------------------------------------------
-- NOTE: < 0 is treated as grow back to max
sugarGrowbackUnits :: Double
sugarGrowbackUnits = 1.0

sugarCapacityRange :: (Double, Double)
sugarCapacityRange = (0.0, 4.0)

sugarEndowmentRange :: (Double, Double)
sugarEndowmentRange = sugarEndowmentRangeBirthing
-- NOTE: this is specified in book page 33 where the initial endowments are set to 5-25
sugarEndowmentRangeStandard :: (Double, Double)
sugarEndowmentRangeStandard = (5.0, 25.0)
-- NOTE: this is specified in book on page 57
sugarEndowmentRangeBirthing :: (Double, Double)
sugarEndowmentRangeBirthing = (50.0, 100.0)

sugarMetabolismRange :: (Double, Double)
sugarMetabolismRange = (1.0, 5.0)

visionRange :: (Int, Int)
visionRange = visionRangeStandard
-- NOTE: set to 1-6 on page 24
visionRangeStandard :: (Int, Int)
visionRangeStandard = (1, 6)

ageRange :: (Double, Double)
ageRange = ageRangeStandard
ageRangeStandard :: (Double, Double)
ageRangeStandard = (60, 100)
ageRangeInf :: (Double, Double)
ageRangeInf = (1/0, 1/0)
------------------------------------------------------------------------------------------------------------------------