{-# LANGUAGE Arrows               #-}
{-# LANGUAGE FlexibleContexts     #-}
module FRP.Chimera.Reactive.DataFlow
  (
    DataSource
  
  , dataFlowOccasionally
  , dataFlowOccasionallySrc
  
  , dataFlowOccasionallySS
  , dataFlowOccasionallySrcSS

  , constDataReceiverSource
  , constDataSource
  , randomNeighbourNodeMsgSource
  , randomNeighbourCellMsgSource
  , randomAgentIdMsgSource
  ) where

import FRP.Chimera.Agent.Interface

type DataSource m o d e = SF m (AgentIn o d e, e) (AgentData d)

-- TODO: dataFlowRepeatedly
-- TODO: dataFlowAfter
-- TODO: dataFlowOnEvent
-- TODO: dataFlowOnDataReceived

dataFlowOccasionally :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                     => Double
                     -> AgentData d
                     -> SF m (AgentIn o d e, e) ()
dataFlowOccasionally rate d = dataFlowOccasionallySrc rate (constDataSource d)

dataFlowOccasionallySrc :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                        => Double
                        -> DataSource m o d e 
                        -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySrc rate dfSrc = proc (ain, e) -> do
  sendEvt <- occasionally rate () -< ()
  if isEvent sendEvt 
    then (do
      d <- dfSrc      -< (ain, e)
      _ <- dataFlowS  -< d
      returnA         -< ())
    else returnA      -< ()

dataFlowOccasionallySS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                       => Double
                       -> Int
                       -> AgentData d
                       -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySS rate ss d = dataFlowOccasionallySrcSS rate ss (constDataSource d)

dataFlowOccasionallySrcSS :: (MonadRandom m, MonadState (AgentOut m o d e) m)
                          => Double
                          -> Int
                          -> DataSource m o d e 
                          -> SF m (AgentIn o d e, e) ()
dataFlowOccasionallySrcSS rate ss dfSrc = proc aie -> do
    sendEvtsSS <- superSamplingUniform ss (occasionally rate ())  -< ()
    dfSS       <- superSamplingUniform ss dfSrc                   -< aie
    _          <- arrM $ mapM dataFlowOccasionallySrcSSAux        -< (zip sendEvtsSS dfSS)
    returnA -< ()
  where
    dataFlowOccasionallySrcSSAux :: MonadState (AgentOut m o d e) m
                                 => (Event (), AgentData d)
                                 -> m ()
    dataFlowOccasionallySrcSSAux (NoEvent, _) = return ()
    dataFlowOccasionallySrcSSAux (Event (), d) = dataFlowM d
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- MESSAGE-Sources
-------------------------------------------------------------------------------
constDataReceiverSource :: Monad m => d -> AgentId -> DataSource m o d e
constDataReceiverSource d receiver = constant (receiver, d)

constDataSource :: Monad m => AgentData d -> DataSource m o d e
constDataSource = constant

randomNeighbourNodeMsgSource :: MonadRandom m
                             => d
                             -> DataSource m o d (Network l)
randomNeighbourNodeMsgSource d = proc (ain, e) -> do
  let aid = agentId ain
  randNode <- arrM (\(aid, e) -> randomNeighbourNode aid e) -< (aid, e)
  returnA -< (randNode, d)

-- NOTE: assumes state isJust
randomNeighbourCellMsgSource :: MonadRandom m
                             => (o -> Discrete2dCoord) 
                             -> d 
                             -> Bool 
                             -> DataSource m o d (Discrete2d AgentId)
randomNeighbourCellMsgSource posFunc d ic = proc (_, e) -> do
  let pos = posFunc $ fromJust $ agentObservable
  randCell <- arrM (\(pos, e) -> randomNeighbourCell pos ic e) -< (pos, e)
  returnA -< (randCell, d)

randomAgentIdMsgSource :: MonadRandom m
                       => d
                       -> Bool 
                       -> DataSource m o d [AgentId]
randomAgentIdMsgSource d ignoreSelf = proc aie@(ain, agentIds) -> do
  let aid = agentId ain
  randAid <- drawRandomElemSF g -< agentIds
  if True == ignoreSelf && aid == randAid
    then randomAgentIdMsgSource (snd $ split g) m ignoreSelf -< aie
    else returnA -< (randAid, d)
-------------------------------------------------------------------------------