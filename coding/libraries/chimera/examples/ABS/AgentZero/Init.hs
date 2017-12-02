module Init 
  (
    initAgentZeroEpstein
  ) where

import Control.Monad.Random
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import FRP.Chimera
import FRP.Yampa

import Agent
import Model

initAgentZeroEpstein :: RandomGen g => Rand g ([AgentZeroAgentDef], AgentZeroEnvironment)
initAgentZeroEpstein = do
  let dims = (33, 33)

  let a0Node = 0
  let a1Node = 1
  let a2Node = 2

  let a0LNode = (a0Node, ()) :: LNode ()
  let a1LNode = (a1Node, ()) :: LNode ()
  let a2LNode = (a2Node, ()) :: LNode ()

  let a0_a1LEdge = (a0Node, a1Node, 0.3) :: LEdge Double
  let a0_a2LEdge = (a0Node, a2Node, 0.3) :: LEdge Double

  -- let a1_a0LEdge = (a1Node, a0Node, 0.3) :: LEdge Double
  let a1_a2LEdge = (a1Node, a2Node, 0.3) :: LEdge Double

  -- let a2_a0LEdge = (a2Node, a0Node, 0.3) :: LEdge Double
  -- let a2_a1LEdge = (a2Node, a1Node, 0.3) :: LEdge Double

  let gr = mkGraph [a0LNode, a1LNode, a2LNode] [a0_a1LEdge, a0_a2LEdge, a1_a2LEdge] :: Gr () Double

  a0 <- agentZero (0, (9, 9))
  a1 <- agentZero (1, (22, 17))
  a2 <- agentZero (2, (20, 26))

  cells <- createCells dims

  let net = createNetworkWithGraph gr 
  let wp = createDiscrete2d
            dims
            neumann -- moore
            ClipToMax
            cells

  let asp = createContinuous2d (disc2dToCont2d dims) ClipToMax

  let e = AgentZeroEnvironment {
    azAgentNetwork = net
  , azAgentSpace = asp
  , azWorldPatches = wp
  }

  return ([a0, a1, a2], e)

agentZero :: RandomGen g => (AgentId, Continuous2dCoord) -> Rand g AgentZeroAgentDef
agentZero (aid, coord) = do
  g <- getSplit
  let s = AgentZeroAgentState {
    azAgentAffect = 0.001
  , azAgentLearningRate = 0.1
  , azAgentLambda = 1.0
  , azAgentDelta = 0.0
  , azAgentThresh = 0.5
  , azAgentEventCount = 0
  , azAgentDispo = 0.0
  , azAgentProb = 0.0
  , azAgentMemory = replicate memorySize 0.0
  , azAgentCoord = coord
  }

  return AgentDef {
    adId = aid
  , adInitMessages = NoEvent
  , adBeh = agentZeroAgentBehaviour g s
  }

createCells :: RandomGen g => Discrete2dDimension -> Rand g [(Discrete2dCoord, AgentZeroEnvCell)]
createCells (maxX, maxY) = mapM randomCell coords
  where
    coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]

    randomCell :: RandomGen g => Discrete2dCoord ->Rand g (Discrete2dCoord, AgentZeroEnvCell)
    randomCell coord = do
      randShade <- getRandomR (0.0, 0.75)

      let c = AgentZeroEnvCell {
        azCellState = Friendly
      , azCellShade = randShade
      }

      return (coord, c)