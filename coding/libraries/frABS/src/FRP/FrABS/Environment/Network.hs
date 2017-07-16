module FRP.FrABS.Environment.Network (
    NetworkType (..),
    Network (..), -- TODO: hide data-constructor

    createNetwork,
    constEdgeLabeler,
    unitEdgeLabeler,

    nodesOfNetwork,
    networkDegrees,
    neighbourNodes,
    neighbourEdges,
    neighbourAgentIds,
    neighbourAgentIdsM,
    neighbourLinks,
    directLinkBetween,
    directLinkBetweenM
  ) where

import FRP.FrABS.Agent.Agent

import FRP.FrABS.Environment.Definitions

import Control.Monad.Random
import Control.Monad.Trans.State

import Data.List
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

-- TODO: how can we introduce labels to edges in an elegant way?
--       problem: creating random-networks is then much more difficult as we need to provide the
--       creation algorithm with some mechanics to derive the labels

type EdgeLabeler l = (AgentId -> AgentId -> l)

data NetworkType = Complete Int | ErdosRenyi Int Double | BarbasiAlbert Int Int Int

data Network l = Network {
    envNetRng :: StdGen,
    envNetGraph :: Gr () l
}

-------------------------------------------------------------------------------
-- Network implementation
-------------------------------------------------------------------------------
createNetwork :: NetworkType 
                    -> EdgeLabeler l
                    -> Rand StdGen (Network l)
createNetwork t l =
    do
        rng <- getSplit
        gr <- createGraph t l

        return Network {
            envNetRng = rng,
            envNetGraph = gr
        }


constEdgeLabeler :: l -> EdgeLabeler l
constEdgeLabeler l _ _ = l

unitEdgeLabeler :: EdgeLabeler ()
unitEdgeLabeler = constEdgeLabeler ()

nodesOfNetwork :: Network l -> [AgentId]
nodesOfNetwork e = nodes gr
    where
        gr = envNetGraph e

networkDegrees :: Network l -> [(AgentId, Int)]
networkDegrees e = degrees gr
    where
        gr = envNetGraph e

neighbourNodes :: AgentId -> Network l -> [AgentId]
neighbourNodes node e = map snd ls
    where
        ls = neighbourLinks node e

neighbourEdges :: AgentId -> Network l ->  [l]
neighbourEdges aid e = map fst ls
    where
        ls = neighbourLinks aid e

neighbourAgentIds :: AgentId -> Network l -> [AgentId]
neighbourAgentIds aid e = map snd ls
    where
        ls = neighbourLinks aid e

neighbourAgentIdsM :: AgentId -> State (Network l) [AgentId]
neighbourAgentIdsM aid = state (\e -> (neighbourAgentIds aid e, e))

neighbourLinks :: AgentId -> (Network l) -> Adj l
neighbourLinks aid e = lneighbors gr aid
    where
        gr = envNetGraph e

directLinkBetween :: AgentId -> AgentId -> (Network l) -> Maybe l
directLinkBetween n1 n2 e = 
    do
        let ls = neighbourLinks n1 e
        (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
        return linkLabel

directLinkBetweenM :: AgentId -> AgentId -> State (Network l) (Maybe l)
directLinkBetweenM n1 n2 = state (\e -> (directLinkBetween n1 n2 e, e))

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
createGraph :: NetworkType -> EdgeLabeler l -> Rand StdGen (Gr () l)
createGraph (Complete n) l = createCompleteGraph l n
createGraph (ErdosRenyi n p) l = createErdosRenyiGraph l n p
createGraph (BarbasiAlbert m0 m n) l = createBarbasiAlbertGraph l n m0 m

createCompleteGraph :: EdgeLabeler l -> Int -> Rand StdGen (Gr () l)
createCompleteGraph l n = 
    do
        let gr = mkGraph nodes edges -- :: Gr () l
        return gr
    where
        nodes = allNodes n
        edges = allEdges l n
        

createErdosRenyiGraph :: EdgeLabeler l -> Int -> Double -> Rand StdGen (Gr () l)
createErdosRenyiGraph l n p = 
    do
        let boundary = (0.0, 1.0) :: (Double, Double) -- sometimes the type-system of Haskell is f**** anyoing...

        infRandomThreshs <- getRandomRs boundary

        let randomThreshs = take (length edges) infRandomThreshs
        let randomEdges = map fst $ filter ((<=p) . snd) (zip edges randomThreshs)

        let gr = mkGraph nodes randomEdges -- :: Gr () l
        return gr

    where
        nodes = allNodes n
        edges = allEdges l n

createBarbasiAlbertGraph :: EdgeLabeler l -> Int -> Int -> Int -> Rand StdGen (Gr () l)
createBarbasiAlbertGraph l n m0 m 
    | m > m0 = error "Cannot create BarbasiAlbert-Graph: m0 <= m violated"
    | otherwise = 
    do
        -- start with a fully connected graph of m0 AgentIds
        initGr <- createCompleteGraph l m0
        let initDegDist = buildDegreeDistr initGr

        -- add (n - m0) AgentIds, where each AgentId is connected to m existing AgentIds where the m existing AgentIds are picked at random
        (finalGr', _) <- foldM (createBarbasiAlbertGraphAux l) (initGr, initDegDist) [m0 .. n - 1]
        return finalGr'

    where
        createBarbasiAlbertGraphAux :: EdgeLabeler l 
                                        -> (Gr () l, [AgentId]) 
                                        -> AgentId 
                                        -> Rand StdGen (Gr () l, [AgentId])
        createBarbasiAlbertGraphAux l (gr, degDistr) aid = 
            do
                randomAgentIds <- pickRandomAgentIds m aid [] degDistr
                let randomEdges = map (\randAgentId -> createEdgeByLabeler l aid randAgentId) randomAgentIds
                
                let gr0 = insNode (aid, ()) gr 
                let gr1 = insEdges randomEdges gr0
                
                let degDistr' = randomAgentIds ++ degDistr
                return (gr1, degDistr')

        pickRandomAgentIds :: Int -> AgentId -> [AgentId] -> [AgentId] -> Rand StdGen [AgentId]
        pickRandomAgentIds 0 _ acc _ = return acc
        pickRandomAgentIds n self acc dist = 
            do
                randIdx <- getRandomR (0, length dist - 1)

                let randAgentId = dist !! randIdx
                -- NOTE: prevent multi-graphs
                let nodeAlreadyPicked = randAgentId `elem` acc
                -- NOTE: prevent self-loops
                let randAgentIdIsSelf = randAgentId == self

                if nodeAlreadyPicked || randAgentIdIsSelf then
                    pickRandomAgentIds n self acc dist 
                    else
                        pickRandomAgentIds (n-1) self (randAgentId : acc) dist

        buildDegreeDistr :: Gr () l -> [AgentId]
        buildDegreeDistr gr = foldr (\(n, count) acc -> replicate count n ++ acc) [] d
            where
                d = degrees gr

createEdgeByLabeler :: EdgeLabeler l -> AgentId -> AgentId -> LEdge l
createEdgeByLabeler l aidFrom aidTo = (aidFrom, aidTo, label) 
    where
        label = l aidFrom aidTo

allNodes :: Int -> [(AgentId, ())]
allNodes n = [ (aid, ()) | aid <- [0..(n-1)]]

allEdges :: EdgeLabeler l -> Int -> [LEdge l]
allEdges l n = [ createEdgeByLabeler l aidFrom aidTo | aidFrom <- [0..(n-2)], aidTo <- [(aidFrom + 1)..(n-1)]]
        
degrees :: Gr () l -> [(AgentId, Int)]
degrees gr = zip ns d
    where
        ns = nodes gr
        d = map (deg gr) ns
-------------------------------------------------------------------------------