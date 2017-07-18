module HeroesCowards.Init (
    createHeroesCowards
  ) where

import HeroesCowards.Agent
import HeroesCowards.Model

import FRP.FrABS

import FRP.Yampa

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Control.Monad.Random

createHeroesCowards :: Int -> IO ([HACAgentDef], HACEnvironment)
createHeroesCowards agentCount = 
    do
        rng <- newStdGen
        gr <- buildGraph agentCount
        let aids = nodes gr

        adefs <- mapM (randomHACDev gr) aids

        let e = createContinuous2d (1.0, 1.0) ClipToMax

        return (adefs, e)

-- TODO: put this into network as: HeroesCowardsGraph: has n nodes where each node is randomly connected to m other nodes
buildGraph :: Int -> IO (Gr () ())
buildGraph agentCount = 
    do
        let aids = [0..agentCount-1]
        let nodes = [ (node, ()) | node <- aids]

        edges <- foldM buildEdge [] aids

        let initGr = mkGraph nodes edges :: Gr () ()

        return initGr

    where
        buildEdge :: [LEdge ()] -> AgentId -> IO [LEdge ()]
        buildEdge accEdge aid = 
            do
                (n1, n2) <- twoRandomNeighbours agentCount aid

                let e1 = ((aid, n1, ()))
                let e2 = ((aid, n2, ()))
                
                return (e1 : e2 : accEdge)

twoRandomNeighbours:: Int -> AgentId -> IO (AgentId, AgentId)
twoRandomNeighbours nodeCount fromNode = 
    do
        randNeigh1 <- randomRIO (0, nodeCount-1)
        randNeigh2 <- randomRIO (0, nodeCount-1)

        if (randNeigh1 == fromNode || 
            randNeigh2 == fromNode || 
            randNeigh1 == randNeigh2) then
            twoRandomNeighbours nodeCount fromNode
            else
                return (randNeigh1, randNeigh2)

randomHACDev :: Gr () () -> AgentId -> IO HACAgentDef 
randomHACDev gr agentId = 
    do
        rng <- newStdGen

        randHero <- randomIO :: IO Bool
        randHeroThresh <- randomRIO (0.0, 1.0) :: IO Double

        randX <- randomRIO (0.0, 1.0)
        randY <- randomRIO (0.0, 1.0)

        let (friend : enemy : _) = map snd (lneighbors gr agentId)

        --let randRole = if randHero then Hero else Coward
        let randRole = if randHeroThresh <= 0.25 then Hero else Coward

        let hacAgentState = HACAgentState {
            hacRole = randRole,
            hacCoord = (randX, randY),
            hacFriendCoord = (0.0, 0.0),
            hacEnemyCoord = (0.0, 0.0),
            hacFriend = friend,
            hacEnemy = enemy
        }

        return AgentDef {
            adId = agentId,
            adState = hacAgentState,
            adConversation = Nothing,
            adInitMessages = NoEvent,
            adBeh = heroesCowardsAgentBehaviour randRole,
            adRng = rng }