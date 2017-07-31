module AgentZero.Init (
    initAgentZeroEpstein
  ) where

import           AgentZero.Agent
import           AgentZero.Environment
import           AgentZero.Model

import           FRP.FrABS

import           FRP.Yampa

import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree

import           Control.Monad.Random

initAgentZeroEpstein :: IO ([AgentZeroAgentDef], AgentZeroEnvironment)
initAgentZeroEpstein =
  do
    let dims = (33, 33)

    let a0Node = 0
    let a1Node = 1
    let a2Node = 2

    let a0LNode = (a0Node, ()) :: LNode ()
    let a1LNode = (a1Node, ()) :: LNode ()
    let a2LNode = (a2Node, ()) :: LNode ()

    let a0_a1LEdge = (a0Node, a1Node, 0.3) :: LEdge Double
    let a0_a2LEdge = (a0Node, a2Node, 0.3) :: LEdge Double

    let a1_a0LEdge = (a1Node, a0Node, 0.3) :: LEdge Double
    let a1_a2LEdge = (a1Node, a2Node, 0.3) :: LEdge Double

    let a2_a0LEdge = (a2Node, a0Node, 0.3) :: LEdge Double
    let a2_a1LEdge = (a2Node, a1Node, 0.3) :: LEdge Double

    let gr = mkGraph [a0LNode, a1LNode, a2LNode] [a0_a1LEdge, a0_a2LEdge, a1_a2LEdge] :: Gr () Double

    let nsA0 = lneighbors gr a0Node
    let nsA1 = lneighbors gr a1Node
    let nsA2 = lneighbors gr a2Node
    print nsA0
    print nsA1
    print nsA2

    a0 <- createAgentZero (0, (9, 9))
    a1 <- createAgentZero (1, (22, 17))
    a2 <- createAgentZero (2, (20, 26))

    cells <- createCells dims

    netRng <- newStdGen
    discRng <- newStdGen

    let net = createNetworkWithGraph gr netRng 
    let wp = createDiscrete2d
                        dims
                        neumann -- moore
                        ClipToMax
                        cells
                        discRng

    let asp = createContinuous2d (disc2dToCont2d dims) ClipToMax

    let e = AgentZeroEnvironment {
      azAgentNetwork = net,
      azAgentSpace = asp,
      azWorldPatches = wp
    }

    return ([a0, a1, a2], e)

createAgentZero :: (AgentId, Continuous2dCoord) -> IO AgentZeroAgentDef
createAgentZero (aid, coord) =
  do
    rng <- newStdGen
    let s = AgentZeroAgentState {
          azAgentAffect = 0.001,
          azAgentLearningRate = 0.1,
          azAgentLambda = 1.0,
          azAgentDelta = 0.0,
          azAgentThresh = 0.5,
          azAgentEventCount = 0,
          azAgentDispo = 0.0,
          azAgentProb = 0.0,
          azAgentMemory = replicate memorySize 0.0,
          azAgentCoord = coord
        }

    return AgentDef {
       adId = aid,
       adState = s,
       adConversation = Nothing,
       adInitMessages = NoEvent,
       adBeh = agentZeroAgentBehaviour,
       adRng = rng }

createCells :: Discrete2dDimension -> IO [(Discrete2dCoord, AgentZeroEnvCell)]
createCells (maxX, maxY) = mapM randomCell coords
  where
    coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]

    randomCell :: Discrete2dCoord -> IO (Discrete2dCoord, AgentZeroEnvCell)
    randomCell coord = do
            randShade <- getStdRandom $ randomR (0.0, 0.75)

            let c = AgentZeroEnvCell {
                azCellState = Friendly,
                azCellShade = randShade
            }

            return (coord, c)