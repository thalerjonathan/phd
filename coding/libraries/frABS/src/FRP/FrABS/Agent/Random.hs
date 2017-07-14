module FRP.FrABS.Agent.Random (
    runAgentRandom,
    runAgentRandomM,
    
    drawRandomRangeFromAgent,
    drawMultipleRandomRangeFromAgent,
    drawBoolWithProbFromAgent,
    drawBoolWithProbFromAgentM,
    splitRandomFromAgent,
    agentPickRandom,
    agentPickRandomM,
    agentPickRandomMultiple,
    agentPickRandomMultipleM,

    drawRandomBool,
    drawRandomBoolM,
    drawRandomExponential,
    drawRandomExponentialM
  ) where

import FRP.FrABS.Agent.Agent

import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

-- NOTE: beware of a = AgentOut (randomly manipulating AgentOut) because one will end up with 2 versions of AgentOut which need to be merged
runAgentRandom :: Rand StdGen a -> AgentOut s m e -> (a, AgentOut s m e)
runAgentRandom f a = (ret, a')
    where
        g = aoRng a
        (ret, g') = runRand f g
        a' = a {aoRng = g'}

runAgentRandomM :: Rand StdGen a -> State (AgentOut s m e) a
runAgentRandomM f = state (runAgentRandomMAux f)
    where
        runAgentRandomMAux :: Rand StdGen a -> AgentOut s m e -> (a, AgentOut s m e)
        runAgentRandomMAux f ao = runAgentRandom f ao

drawRandomRangeFromAgent :: (Random a) => (a, a) -> AgentOut s m e -> (a, AgentOut s m e)
drawRandomRangeFromAgent r a = runAgentRandom (getRandomR r) a 

drawMultipleRandomRangeFromAgent :: (Random a) => (a, a) -> Int -> AgentOut s m e -> ([a], AgentOut s m e)
drawMultipleRandomRangeFromAgent r n a = runAgentRandom blub a 
    where
        blub = do
                infRand <- getRandomRs r
                let nRand = take n infRand
                return nRand

drawBoolWithProbFromAgent :: Double -> AgentOut s m e -> (Bool, AgentOut s m e)
drawBoolWithProbFromAgent p ao = (trueFlag, ao')
    where
        (r, ao') = drawRandomRangeFromAgent (0.0, 1.0) ao
        trueFlag = p >= r

drawBoolWithProbFromAgentM :: Double -> State (AgentOut s m e) Bool
drawBoolWithProbFromAgentM p = state drawBoolWithProbFromAgentMAux 
    where
        drawBoolWithProbFromAgentMAux :: (AgentOut s m e) -> (Bool, AgentOut s m e)
        drawBoolWithProbFromAgentMAux ao = drawBoolWithProbFromAgent p ao

splitRandomFromAgent :: AgentOut s m e -> (StdGen, AgentOut s m e)
splitRandomFromAgent a = runAgentRandom getSplit a 

agentPickRandom :: [a] -> AgentOut s m e -> (a, AgentOut s m e)
agentPickRandom xs a 
    | null xs = error "cannot draw single random element from empty list"
    | otherwise = (randElem, a')
    where
        cellCount = length xs
        (randIdx, a') = drawRandomRangeFromAgent (0, cellCount - 1) a 
        randElem = xs !! randIdx

agentPickRandomM :: [a] -> State (AgentOut s m e) a
agentPickRandomM xs = state (\ao -> agentPickRandom xs ao)

agentPickRandomMultiple :: [a] -> Int -> AgentOut s m e -> ([a], AgentOut s m e)
agentPickRandomMultiple xs n a 
    | null xs = error "cannot draw multiple random elements from empty list"
    | otherwise = (randElems, a')
    where
        cellCount = length xs
        (randIndices, a') = drawMultipleRandomRangeFromAgent (0, cellCount - 1) n a 
        randElems = foldr (\idx acc -> (xs !! idx) : acc) [] randIndices  

agentPickRandomMultipleM :: [a] -> Int -> State (AgentOut s m e) [a]
agentPickRandomMultipleM xs n = state (\ao -> agentPickRandomMultiple xs n ao)

drawRandomBoolM :: (RandomGen g) => Double -> Rand g Bool
drawRandomBoolM p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

drawRandomBool :: (RandomGen g) => g -> Double -> (Bool, g)
drawRandomBool g p = runRand (drawRandomBoolM p) g

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
drawRandomExponentialM :: (RandomGen g) => Double -> Rand g Double
drawRandomExponentialM lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))

drawRandomExponential :: (RandomGen g) => g -> Double -> (Double, g)
drawRandomExponential g lambda = runRand (drawRandomExponentialM lambda) g

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoid :: (Random a, Eq a, RandomGen g) => a -> Rand g a
avoid x = 
    do
        r <- getRandom
        if (r == x) then
            avoid x
            else
                return r