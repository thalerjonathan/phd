module SugarScape.SugarScapeAgentChapter2 where

-- Project-internal import first
import SugarScape.SugarScapeModel
import SugarScape.SugarScapeEnvironment
import FrABS.Env.Environment
import FrABS.Agent.Agent

-- Project-specific libraries follow
import FRP.Yampa

-- System imports then
import Data.Maybe
import Data.List

-- debugging imports finally, to be easily removed in final version
import Debug.Trace
import System.Random

