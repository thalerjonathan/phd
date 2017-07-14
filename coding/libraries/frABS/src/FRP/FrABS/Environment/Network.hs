module FRP.FrABS.Environment.Network (
    NetworkType (..),
    NetworkEnvironmentData (..),

    createAgentNetwork,
    EnvironmentNetwork (..)
  ) where

import FRP.FrABS.Environment.Definitions

import Control.Monad.Random

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

data NetworkType = Complete Int | ErdosRenyi Int Double | BarbasiAlbert Int Int Int

data NetworkEnvironmentData = NetworkEnvironmentData l {
    envBehaviour :: Maybe (EnvironmentBehaviour (NetworkEnvironmentData l)),
    envRng :: StdGen,
    envGraph :: EnvGraph l
}

createAgentNetwork :: NetworkType -> Rand StdGen (Gr () ())
createAgentNetwork (Complete n) = createCompleteGraph n
createAgentNetwork (ErdosRenyi n p) = createErdosRenyiGraph n p
createAgentNetwork (BarbasiAlbert m0 m n) = createBarbasiAlbertGraph n m0 m

-------------------------------------------------------------------------------
-- Environment-Instance implementation
-------------------------------------------------------------------------------
instance EnvironmentNetwork (NetworkEnvironmentData l) where
    -- environmentBehaviour :: Maybe (EnvironmentBehaviour e)
    environmentBehaviour = beh
        where
            beh = envBehaviour e

    -- networkDegrees :: e -> [Node]
    nodesOfNetwork e = nodes gr
        where
            gr = envGraph e

    -- networkDegrees :: e -> [(Node, Int)]
    networkDegrees e = zip ns d
        where
            gr = envGraph e
            ns = nodes gr
            d = map (deg gr) ns

    -- neighbourEdges :: Node -> e ->  [l]
    neighbourEdges node e = map fst ls
        where
            ls = neighbourLinks node e

    -- neighbourNodes :: Node -> e -> [Node]
    neighbourNodes node e = map snd ls
        where
            ls = neighbourLinks node e

    -- neighbourNodesM :: Node -> State e [Node]
    neighbourNodesM node = state (\e -> (neighbourNodes node e, e))

    -- neighbourLinks :: Node -> e -> Adj l
    neighbourLinks node e = lneighbors gr node
        where
            gr = envGraph e

    -- directLinkBetween :: Node -> Node -> e -> Maybe l
    directLinkBetween n1 n2 e = 
        do
            let ls = neighbourLinks n1 e
            (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
            return linkLabel

    -- directLinkBetweenM :: Node -> Node -> State e (Maybe l)
    directLinkBetweenM n1 n2 = state (\e -> (directLinkBetween n1 n2 e, e))

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
createCompleteGraph :: Int -> Rand StdGen (Gr () ())
createCompleteGraph n = 
    do
        let gr = mkGraph nodes edges :: Gr () ()
        return gr
    where
        edges = [ (nodeFrom, nodeTo, ()) | nodeFrom <- [0..(n-2)], nodeTo <- [(nodeFrom + 1)..(n-1)]]
        nodes = [ (node, ()) | node <- [0..(n-1)]]

createErdosRenyiGraph :: Int -> Double -> Rand StdGen (Gr () ())
createErdosRenyiGraph n p = 
    do
        let nodes = [ (node, ()) | node <- [0..(n-1)]]
        let allEdges = [ (nodeFrom, nodeTo, ()) | nodeFrom <- [0..(n-2)], nodeTo <- [(nodeFrom + 1)..(n-1)]]
        let boundary = (0.0, 1.0) :: (Double, Double) -- sometimes the type-system of Haskell is f**** anyoing...

        infRandomThreshs <- getRandomRs boundary

        let randomThreshs = take (length allEdges) infRandomThreshs
        let randomEdges = map fst $ filter ((<=p) . snd) (zip allEdges randomThreshs)

        let gr = mkGraph nodes randomEdges :: Gr () ()

        return gr

createBarbasiAlbertGraph :: Int -> Int -> Int -> Rand StdGen (Gr () ())
createBarbasiAlbertGraph n m0 m 
    | m > m0 = error "Cannot create BarbasiAlbert-Graph: m0 <= m violated"
    | otherwise = 
    do
        -- start with a fully connected graph of m0 nodes
        initGr <- createCompleteGraph m0
        let initDegDist = buildDegreeDistr initGr

        -- add (n - m0) nodes, where each node is connected to m existing nodes where the m existing nodes are picked at random
        (finalGr', _) <- foldM createBarbasiAlbertGraphAux (initGr, initDegDist) [m0 .. n - 1]

        return finalGr'

    where
        createBarbasiAlbertGraphAux :: (Gr () (), [Node]) -> Node -> Rand StdGen (Gr () (), [Node])
        createBarbasiAlbertGraphAux (gr, degDistr) node = 
            do
                randomNodes <- pickRandomNodes m node [] degDistr
                let randomEdges = map (\randNode -> (node, randNode, ())) randomNodes
                
                let gr0 = insNode (node, ()) gr 
                let gr1 = insEdges randomEdges gr0
                
                let degDistr' = randomNodes ++ degDistr
                return (gr1, degDistr')

        pickRandomNodes :: Int -> Node -> [Node] -> [Node] -> Rand StdGen [Node]
        pickRandomNodes 0 _ acc _ = return acc
        pickRandomNodes n self acc dist = 
            do
                randIdx <- getRandomR (0, length dist - 1)

                let randNode = dist !! randIdx
                -- NOTE: prevent multi-graphs
                let nodeAlreadyPicked = randNode `elem` acc
                -- NOTE: prevent self-loops
                let randNodeIsSelf = randNode == self

                if nodeAlreadyPicked || randNodeIsSelf then
                    pickRandomNodes n self acc dist 
                    else
                        pickRandomNodes (n-1) self (randNode : acc) dist

        buildDegreeDistr :: Gr () () -> [Node]
        buildDegreeDistr gr = foldr (\(n, count) acc -> replicate count n ++ acc) [] d
            where
                d = networkDegrees gr
-------------------------------------------------------------------------------