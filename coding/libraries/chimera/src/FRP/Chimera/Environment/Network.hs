module FRP.Chimera.Environment.Network 
  (
    NetworkType (..)
  , DeterministicNetwork (..)
  , RandomNetwork (..)
  , Network (..)

  , createNetwork
  , createDeterministicNetwork
  , createRandomNetwork
  , createEmptyNetwork
  , createNetworkWithGraph

  , constEdgeLabeler
  , unitEdgeLabeler

  , nodesOfNetwork
  , networkDegrees
  , neighbourNodes
  , neighbourEdges
  , neighbourAgentIds
  , neighbourAgentIdsM
  , neighbourLinks
  , directLinkBetween
  , directLinkBetweenM

  , randomNeighbourNode
  ) where

import Data.List

import Control.Monad.Random
import Control.Monad.Trans.State
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import FRP.Chimera.Agent.Interface

type EdgeLabeler l          = (AgentId -> AgentId -> l)
data NetworkType            = Random RandomNetwork | Deterministic DeterministicNetwork
data DeterministicNetwork   = Complete Int
data RandomNetwork          = ErdosRenyi Int Double | BarbasiAlbert Int Int Int

data Network l = Network 
  {
    envNetGraph :: Gr () l
  } deriving (Show, Read)

-- NOTE: the underlying graph-library (FGL) can't deal with a fully-connected graph
-- which is too big (>= 10.000 nodes) as the connections between the links
-- grow exponentially with the number of nodes and consumes extreme amount of memory
-- I killed a machine with 16GByte of memory when constructing a fully-connected graph
-- with 10.000 nodes. 
-- When wanting full connectivity with big number of nodes, use simply an array as environment

-------------------------------------------------------------------------------
-- Network implementation
-------------------------------------------------------------------------------
createNetwork :: RandomGen g =>
                NetworkType 
                -> EdgeLabeler l
                -> Rand g (Network l)
createNetwork (Deterministic t) l = return $ createDeterministicNetwork t l
createNetwork (Random t) l = createRandomNetwork t l

createDeterministicNetwork :: DeterministicNetwork 
                              -> EdgeLabeler l
                              -> Network l
createDeterministicNetwork (Complete n) l = Network { envNetGraph = gr }
  where
    gr = createCompleteGraph l n

createRandomNetwork :: RandomGen g =>
                      RandomNetwork 
                      -> EdgeLabeler l
                      -> Rand g (Network l)
createRandomNetwork (ErdosRenyi n p) l = createErdosRenyiGraph l n p >>= (\gr -> return Network { envNetGraph = gr })
createRandomNetwork (BarbasiAlbert m0 m n) l = createBarbasiAlbertGraph l n m0 m >>= (\gr -> return Network { envNetGraph = gr })

createEmptyNetwork :: Network l
createEmptyNetwork = Network { envNetGraph = empty }

createNetworkWithGraph :: Gr () l -> Network l
createNetworkWithGraph gr = Network { envNetGraph = gr }

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
neighbourAgentIds = neighbourNodes

neighbourAgentIdsM :: AgentId -> State (Network l) [AgentId]
neighbourAgentIdsM aid = state (\e -> (neighbourAgentIds aid e, e))

neighbourLinks :: AgentId -> Network l -> Adj l
neighbourLinks aid e = lneighbors gr aid
  where
    gr = envNetGraph e

directLinkBetween :: AgentId -> AgentId -> Network l -> Maybe l
directLinkBetween n1 n2 e = do
  let ls = neighbourLinks n1 e
  (linkLabel, _) <- Data.List.find ((==n2) . snd) ls
  return linkLabel

directLinkBetweenM :: AgentId -> AgentId -> State (Network l) (Maybe l)
directLinkBetweenM n1 n2 = state (\e -> (directLinkBetween n1 n2 e, e))

-------------------------------------------------------------------------------
-- UTILITIES
-------------------------------------------------------------------------------
randomNeighbourNode :: RandomGen g => AgentId -> Network l -> Rand g AgentId
randomNeighbourNode aid e = do
  let nn = neighbourNodes aid e
  let l = length nn 
  randIdx <- getRandomR (0, l - 1)
  return (nn !! randIdx)
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- PRIVATE
-------------------------------------------------------------------------------
createCompleteGraph :: EdgeLabeler l -> Int -> Gr () l
createCompleteGraph l n = mkGraph nodes edges
  where
    nodes = allNodes n
    edges = allEdges l n

{-
createCompleteGraph :: EdgeLabeler l -> Int -> Gr () l
createCompleteGraph l n = foldr (createCompleteGraphAux l) gr [0..(n - 1)]
    where
        -- NOTE: need to insert nodes this way, because when using empty list [] as well, haskell will infer a different type
        -- which will result in a directed graph (i think this is one of the very few examples where i got really f***** up by the type-system)
        nodes = allNodes n
        gr = mkGraph nodes []

        createCompleteGraphAux :: EdgeLabeler l 
                                    -> AgentId 
                                    -> Gr () l 
                                    -> Gr () l
        createCompleteGraphAux l aid gr = insEdges edges gr
            where
                edges = map (createEdgeByLabeler l aid) [0.. (aid - 1)]
-}
createErdosRenyiGraph :: RandomGen g => 
                        EdgeLabeler l 
                        -> Int 
                        -> Double 
                        -> Rand g (Gr () l)
createErdosRenyiGraph l n p = do
    let boundary = (0.0, 1.0) :: (Double, Double) -- sometimes the type-system of Haskell is f**** anyoing...

    infRandomThreshs <- getRandomRs boundary

    let randomThreshs = take (length edges) infRandomThreshs
    let randomEdges = map fst $ filter ((<=p) . snd) (zip edges randomThreshs)

    let gr = mkGraph nodes randomEdges -- :: Gr () l
    return gr
  where
    nodes = allNodes n
    edges = allEdges l n

createBarbasiAlbertGraph :: RandomGen g =>
                            EdgeLabeler l 
                            -> Int 
                            -> Int 
                            -> Int 
                            -> Rand g (Gr () l)
createBarbasiAlbertGraph l n m0 m 
    | m > m0 = error "Cannot create BarbasiAlbert-Graph: m0 <= m violated"
    | otherwise = do
      -- start with a fully connected graph of m0 AgentIds
      let initGr = createCompleteGraph l m0
      let initDegDist = buildDegreeDistr initGr

      -- add (n - m0) AgentIds, where each AgentId is connected to m existing AgentIds where the m existing AgentIds are picked at random
      (finalGr', _) <- foldM (createBarbasiAlbertGraphAux l) (initGr, initDegDist) [m0 .. n - 1]
      return finalGr'
  where
    createBarbasiAlbertGraphAux :: RandomGen g =>
                                  EdgeLabeler l 
                                  -> (Gr () l, [AgentId]) 
                                  -> AgentId 
                                  -> Rand g (Gr () l, [AgentId])
    createBarbasiAlbertGraphAux l (gr, degDistr) aid = do
      randomAgentIds <- pickRandomAgentIds m aid [] degDistr
      let randomEdges = map (\randAgentId -> createEdgeByLabeler l aid randAgentId) randomAgentIds
      
      let gr0 = insNode (aid, ()) gr 
      let gr1 = insEdges randomEdges gr0
      
      let degDistr' = randomAgentIds ++ degDistr
      return (gr1, degDistr')

    pickRandomAgentIds :: RandomGen g => 
                          Int 
                          -> AgentId 
                          -> [AgentId] 
                          -> [AgentId] 
                          -> Rand g [AgentId]
    pickRandomAgentIds 0 _ acc _ = return acc
    pickRandomAgentIds n self acc dist =  do
      randIdx <- getRandomR (0, length dist - 1)

      let randAgentId = dist !! randIdx
      -- NOTE: prevent multi-graphs
      let nodeAlreadyPicked = randAgentId `elem` acc
      -- NOTE: prevent self-loops
      let randAgentIdIsSelf = randAgentId == self

      if nodeAlreadyPicked || randAgentIdIsSelf 
        then pickRandomAgentIds n self acc dist 
        else pickRandomAgentIds (n-1) self (randAgentId : acc) dist

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