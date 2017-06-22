module Segregation.SegregationStats where

import Segregation.SegregationModel
import Segregation.SegregationAgent

import FrABS.Agent.Agent

totalSatisfaction :: [SegAgentOut] -> Double
totalSatisfaction aos = sum $ map (segSatisfactionLevel . aoState) aos

satisfactionStats :: [SegAgentOut] -> (Int, Int, Int, Double)
satisfactionStats aos = (totalCount, happyCount, unhappyCount, unhappyFract)
    where
        totalCount = length aos
        happy = filter isSatisfied aos
        happyCount = length happy
        unhappyCount = totalCount - happyCount
        unhappyFract = (fromInteger $ fromIntegral unhappyCount) / (fromInteger $ fromIntegral totalCount)