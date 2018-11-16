module SugarScape.Core.Model 
  ( AgentGender (..)
  , CultureTag
  , AgentTribe (..)
  , TradeInfo (..)
  , Loan (..)
  
  , SugAgentState (..)
  , SugAgentObservable (..)

  , SugEnvSiteOccupier (..)
  , SugEnvSite (..)

  , TradingRefuse (..)
  , TradingReply (..)
  , LoanRefuse (..)
  , LoanReply (..)
  , SugEvent (..)
  
  , SugAgentMonad
  , SugAgentMonadT

  , SugEnvironment

  , SugAgentMSF
  , SugAgentDef
  , SugAgentOut

  , maxSugarCapacitySite
  , maxSpiceCapacitySite

  , sugarscapeDimensions
  
  , envSpec
  ) where

import Control.Monad.Random
import Control.Monad.State.Strict

import SugarScape.Agent.Interface
import SugarScape.Core.Common
import SugarScape.Core.Discrete

------------------------------------------------------------------------------------------------------------------------
-- AGENT-DEFINITIONS
------------------------------------------------------------------------------------------------------------------------
data AgentGender = Male | Female deriving (Show, Eq)
type CultureTag  = [Bool]
data AgentTribe  = Blue | Red deriving (Show, Eq)
data TradeInfo   = TradeInfo Double Double Double AgentId deriving (Show, Eq) -- price, sugar, spice, trade-partner
data Loan      = Loan Time AgentId Double Double deriving (Show, Eq)  -- dueDate, borrower/lender, sugar, spice

data SugAgentState = SugAgentState 
  { sugAgCoord        :: !Discrete2dCoord
  , sugAgSugarMetab   :: !Int               -- integer because discrete, otherwise no exact replication possible
  , sugAgVision       :: !Int
  , sugAgSugarLevel   :: !Double            -- floating point because regrow-rate can be set to floating point values
  , sugAgAge          :: !Int
  , sugAgMaxAge       :: !(Maybe Int)
  
  -- Chapter III properties
  , sugAgGender       :: !AgentGender
  , sugAgFertAgeRange :: !(Int, Int)        -- from, to
  , sugAgInitSugEndow :: !Double
  , sugAgChildren     :: ![AgentId]         -- list of all children the agent has given birth to (together with another agent of opposing sex)
  , sugAgCultureTag   :: !CultureTag
  , sugAgTribe        :: !AgentTribe
  
  -- Chapter IV properties
  , sugAgSpiceLevel   :: !Double            -- floating point because regrow-rate can be set to floating point values
  , sugAgInitSpiEndow :: !Double
  , sugAgSpiceMetab   :: !Int               -- integer because discrete, otherwise no exact replication possible
  
  , sugAgBorrowed     :: ![Loan]    -- contains the Loans the agent has borrowed from the lenders
  , sugAgLent         :: ![Loan]    -- contains the Loans the agent has lent out to borrowers
  , sugAgNetIncome    :: !Double            -- net income of sugar and spice in the most recent step
  } deriving (Show, Eq)

data SugAgentObservable = SugAgentObservable
  { sugObsCoord      :: !Discrete2dCoord
  , sugObsVision     :: !Int
  , sugObsAge        :: !Int
  , sugObsSugLvl     :: !Double
  , sugObsSugMetab   :: !Int

  -- Chapter III properties
  , sugObsGender     :: !AgentGender
  , sugObsCultureTag :: !CultureTag
  , sugObsTribe      :: !AgentTribe

  -- Chapter IV properties
  , sugObsSpiLvl     :: !Double
  , sugObsSpiMetab   :: !Int
  , sugObsTrades     :: [TradeInfo]
  } deriving (Show, Eq)

data SugEnvSiteOccupier = SugEnvSiteOccupier 
  { sugEnvOccId          :: !AgentId
  , sugEnvOccSugarWealth :: !Double
  , sugEnvOccSpiceWealth :: !Double
  , sugEnvOccTribe       :: !AgentTribe
  , sugEnvOccMRS         :: !Double
  } deriving (Show, Eq)

data SugEnvSite = SugEnvSite 
  { sugEnvSiteSugarCapacity :: !Double
  , sugEnvSiteSugarLevel    :: !Double

  , sugEnvSiteSpiceCapacity :: !Double
  , sugEnvSiteSpiceLevel    :: !Double

  , sugEnvSitePolutionLevel :: !Double
  , sugEnvSiteOccupier      :: !(Maybe SugEnvSiteOccupier)
  } deriving (Show, Eq)

data TradingRefuse = NoWelfareIncrease  -- refuse trade because no increase in welfare
                   | MRSCrossover       -- refuse trade because MRS cross-over
                   deriving (Show, Eq)

data TradingReply = AcceptTrade        
                  | RefuseTrade TradingRefuse 
                  deriving (Show, Eq)

data LoanRefuse = NotFertileAge
                  | EnoughWealth
                  | NotLoanWorthy
                  deriving (Show, Eq)

data LoanReply = AcceptLoan
                 | RefuseLoan LoanRefuse
                 deriving (Show, Eq)

data SugEvent = MatingRequest AgentGender
              | MatingReply (Maybe (Double, Double, Int, Int, CultureTag)) -- in case of acceptance: Just share of sugar, spice, metab, vision
              | MatingTx AgentId
              | MatingContinue

              | Inherit Double 

              | CulturalProcess CultureTag

              | KilledInCombat 

              | TradingOffer Double Double  -- offering agent sends MRS before and after trade so receiving agent can turn down if MRS cross-over
              | TradingReply TradingReply

              | LoanOffer Loan
              | LoanReply LoanReply
              | LoanPayback Loan Double Double --  sugarBack, spiceBack
              | LoanLenderDied [AgentId]
              | LoanInherit Loan
              deriving (Show, Eq)

type SugEnvironment = Discrete2d SugEnvSite

type SugAgentMonad g  = StateT SugEnvironment (Rand g)
type SugAgentMonadT g = AgentT (SugAgentMonad g)

type SugAgentMSF g = AgentMSF (SugAgentMonad g) SugEvent SugAgentObservable
type SugAgentDef g = AgentDef (SugAgentMonad g) SugEvent SugAgentObservable
type SugAgentOut g = AgentOut (SugAgentMonad g) SugEvent SugAgentObservable
------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------
-- SUGARSCAPE PARAMETRS
------------------------------------------------------------------------------------------------------------------------
maxSugarCapacitySite :: Int
maxSugarCapacitySite = 4

maxSpiceCapacitySite :: Int
maxSpiceCapacitySite = 4

-- the sugarscape is 51x51 in our implementation
sugarscapeDimensions :: Discrete2dCoord
sugarscapeDimensions = (51, 51)

-- taken from Iain Weaver Sugarscape implementation
-- https://www2.le.ac.uk/departments/interdisciplinary-science/research/replicating-sugarscape
-- http://ccl.northwestern.edu/netlogo/models/community/
envSpec :: [String]
envSpec =
  [ "111111111111111111111111111112222222222111111111111"
  , "111111111111111111111111111222222222222222111111111"
  , "111111111111111111111111112222222222222222221111111"
  , "111111111111111111111111122222222222222222222211111"
  , "111111111111111111111111222222222222222222222221111"
  , "111110000000111111111111222222222223332222222222111"
  , "111110000000001111111111222222223333333332222222111"
  , "111110000000000111111112222222333333333333222222211"
  , "111110000000000111111112222223333333333333322222211"
  , "111110000000000011111112222223333333333333332222221"
  , "111110000000000011111122222233333344444333333222221"
  , "111110000000000111111122222233333444444433333222221"
  , "111111000000000111111122222333334444444443333222222"
  , "111111000000001111111222222333334444444443333322222"
  , "111111100000011111111222222333334444444443333322222"
  , "111111111001111111111222222333334444444443333322222"
  , "111111111111111111111222222333334444444443333222222"
  , "111111111111111111112222222333333444444433333222222"
  , "111111111111111111112222222233333344444333333222222"
  , "111111111111111111122222222233333333333333332222222"
  , "111111111111111112222222222223333333333333332222222"
  , "111111111111122222222222222223333333333333322222222"
  , "111111111122222222222222222222233333333332222222221"
  , "111111122222222222222222222222222333333222222222221"
  , "111122222222222222222222222222222222222222222222211"
  , "111222222222222222222222222222222222222222222222111"
  , "112222222222222222222222222222222222222222222221111"
  , "122222222222333333222222222222222222222222221111111"
  , "122222222233333333332222222222222222222221111111111"
  , "222222223333333333333322222222222222221111111111111"
  , "222222233333333333333322222222222211111111111111111"
  , "222222233333333333333332222222221111111111111111111"
  , "222222333333444443333332222222211111111111111111111"
  , "222222333334444444333333222222211111111111111111111"
  , "222222333344444444433333222222111111111111111111111"
  , "222223333344444444433333222222111111111100111111111"
  , "222223333344444444433333222222111111110000001111111"
  , "222223333344444444433333222222111111100000000111111"
  , "222222333344444444433333222221111111000000000111111"
  , "122222333334444444333332222221111111000000000011111"
  , "122222333333444443333332222221111110000000000011111"
  , "122222233333333333333322222211111110000000000011111"
  , "112222223333333333333322222211111111000000000011111"
  , "112222222333333333333222222211111111000000000011111"
  , "111222222233333333322222222111111111100000000011111"
  , "111222222222233322222222222111111111111000000011111"
  , "111122222222222222222222222111111111111111111111111"
  , "111112222222222222222222221111111111111111111111111"
  , "111111122222222222222222211111111111111111111111111"
  , "111111111222222222222222111111111111111111111111111"
  , "111111111111222222222211111111111111111111111111111"
  ]

