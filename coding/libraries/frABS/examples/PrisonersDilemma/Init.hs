module PrisonersDilemma.Init (
    initPrisonersDilemma
  ) where

import PrisonersDilemma.Model
import PrisonersDilemma.Agent

import FRP.Yampa

import FRP.FrABS

import System.Random
import Control.Monad.Random

initPrisonersDilemma :: Discrete2dDimension -> IO ([PDAgentDef], PDEnvironment)
initPrisonersDilemma dims@(maxX, maxY) = 
  do
    let coords = [ (x, y) | x <- [0..maxX-1], y <- [0..maxY-1] ]
    let agentCount = maxX * maxY

    let aids = [0..agentCount-1]
    let aidCoordPairs = zip aids coords
    let cells = zip coords aids

    let centerX = floor $ (fromIntegral maxX) * 0.5
    let centerY = floor $ (fromIntegral maxY) * 0.5

    envRng <- newStdGen 
    let e = createDiscrete2d
              dims
              moore 
              ClipToMax
              cells
              envRng

    adefs <- mapM (createPDAgent e (centerX, centerY)) aidCoordPairs

    return (adefs, e)

createPDAgent :: PDEnvironment
                  -> Discrete2dCoord
                  -> (AgentId, Discrete2dCoord) 
                  -> IO PDAgentDef
createPDAgent e center (agentId, coord) = 
  do
    rng <- newStdGen

    let a = if center == coord then Defector else Cooperator

    let s = PDAgentState {
              pdCurrAction = a,
              pdPrevAction = a,
              pdLocalPo = 0.0,
              pdBestPo = (a, 0.0),
              pdCoord = coord
            }

    return AgentDef {
      adId = agentId,
      adState = s,
      adConversation = Nothing,
      adInitMessages = NoEvent,
      adBeh = pdAgentBehaviour e,
      adRng = rng
    }