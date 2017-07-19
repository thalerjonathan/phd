module Segregation.Stats (
    totalSatisfaction,
    satisfactionStats
  ) where

import Segregation.Model
import Segregation.Agent

import FRP.FrABS

totalSatisfaction :: [SegAgentState] -> Double
totalSatisfaction ss = sum $ map segSatisfactionLevel ss

satisfactionStats :: [SegAgentState] -> (Int, Int, Int, Double)
satisfactionStats ss = (totalCount, happyCount, unhappyCount, unhappyFract)
    where
        totalCount = length ss
        happy = filter isSatisfiedState ss
        happyCount = length happy
        unhappyCount = totalCount - happyCount
        unhappyFract = (fromInteger $ fromIntegral unhappyCount) / (fromInteger $ fromIntegral totalCount)