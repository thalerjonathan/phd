module FrABS.Agent.AgentRandom (
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
    drawRandomExponential
  ) where

import FrABS.Agent.Agent

import System.Random
import Control.Monad.Random
import Control.Monad.Trans.State

-- NOTE: beware of a = AgentOut (randomly manipulating AgentOut) because one will end up with 2 versions of AgentOut which need to be merged
runAgentRandom :: AgentOut s m ec l -> Rand StdGen a -> (a, AgentOut s m ec l)
runAgentRandom a f = (ret, a')
    where
        g = aoRng a
        (ret, g') = runRand f g
        a' = a {aoRng = g'}

runAgentRandomM :: Rand StdGen a -> State (AgentOut s m ec l) a
runAgentRandomM f = state (runAgentRandomMAux f)
    where
        runAgentRandomMAux :: Rand StdGen a -> AgentOut s m ec l -> (a, AgentOut s m ec l)
        runAgentRandomMAux f ao = runAgentRandom ao f

drawRandomRangeFromAgent :: (Random a) => AgentOut s m ec l -> (a, a) -> (a, AgentOut s m ec l)
drawRandomRangeFromAgent a r = runAgentRandom a (getRandomR r)

drawMultipleRandomRangeFromAgent :: (Random a) => AgentOut s m ec l -> (a, a) -> Int -> ([a], AgentOut s m ec l)
drawMultipleRandomRangeFromAgent a r n = runAgentRandom a blub
    where
        blub = do
                infRand <- getRandomRs r
                let nRand = take n infRand
                return nRand

drawBoolWithProbFromAgent :: AgentOut s m ec l -> Double -> (Bool, AgentOut s m ec l)
drawBoolWithProbFromAgent ao p = (trueFlag, ao')
    where
        (r, ao') = drawRandomRangeFromAgent ao (0.0, 1.0)
        trueFlag = p >= r

drawBoolWithProbFromAgentM :: Double -> State (AgentOut s m ec l) Bool
drawBoolWithProbFromAgentM p = state drawBoolWithProbFromAgentMAux 
    where
        drawBoolWithProbFromAgentMAux :: (AgentOut s m ec l) -> (Bool, AgentOut s m ec l)
        drawBoolWithProbFromAgentMAux ao = drawBoolWithProbFromAgent ao p

splitRandomFromAgent :: AgentOut s m ec l -> (StdGen, AgentOut s m ec l)
splitRandomFromAgent a = runAgentRandom a getSplit

agentPickRandom :: AgentOut s m ec l -> [a] -> (a, AgentOut s m ec l)
agentPickRandom a xs
    | null xs = error "cannot draw single random element from empty list"
    | otherwise = (randElem, a')
    where
        cellCount = length xs
        (randIdx, a') = drawRandomRangeFromAgent a (0, cellCount - 1)
        randElem = xs !! randIdx

agentPickRandomM :: [a] -> State (AgentOut s m ec l) a
agentPickRandomM xs = state (\ao -> agentPickRandom ao xs)

agentPickRandomMultiple :: AgentOut s m ec l -> [a] -> Int -> ([a], AgentOut s m ec l)
agentPickRandomMultiple a xs n
    | null xs = error "cannot draw multiple random elements from empty list"
    | otherwise = (randElems, a')
    where
        cellCount = length xs
        (randIndices, a') = drawMultipleRandomRangeFromAgent a (0, cellCount - 1) n
        randElems = foldr (\idx acc -> (xs !! idx) : acc) [] randIndices  

agentPickRandomMultipleM :: [a] -> Int -> State (AgentOut s m ec l) [a]
agentPickRandomMultipleM xs n = state (\ao -> agentPickRandomMultiple ao xs n)

drawRandomBool :: Double -> Rand StdGen Bool
drawRandomBool p = getRandomR (0.0, 1.0) >>= (\r -> return $ p >= r)

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
drawRandomExponential :: Double -> Rand StdGen Double
drawRandomExponential lambda = avoid 0 >>= (\r -> return $ ((-log r) / lambda))

-- NOTE: THIS CODE INSPIRED BY Euterpea-1.0.0 (I didn't want to create dependencies and their implementation seems neat and tidy)
avoid :: (Random a, Eq a) => a -> Rand StdGen a
avoid x = 
    do
        r <- getRandom
        if (r == x) then
            avoid x
            else
                return r