module Segregation.SegregationStats where

import Segregation.SegregationModel

import FrABS.Agent.Agent

calculateSimilarityChange :: [SegAgentOut] -> [SegAgentOut] -> Double
calculateSimilarityChange aoutsPrev aoutCurr = sumSimilarityCurr - sumSimilarityPrev
    where
        sumSimilarityPrev = totalCurrentSimliarity aoutsPrev
        sumSimilarityCurr = totalCurrentSimliarity aoutCurr

totalCurrentSimliarity :: [SegAgentOut] -> Double
totalCurrentSimliarity aouts = sum $ map (segSimilarityCurrent . aoState) aouts

calculateHappinessStats :: [SegAgentOut] -> (Int, Int, Int, Double)
calculateHappinessStats aouts = (totalCount, happyCount, unhappyCount, unhappyFract)
    where
        totalCount = length aouts
        happy = filter isHappy aouts
        happyCount = length happy
        unhappyCount = totalCount - happyCount
        unhappyFract = (fromInteger $ fromIntegral unhappyCount) / (fromInteger $ fromIntegral totalCount)