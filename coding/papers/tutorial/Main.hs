{-# LANGUAGE Arrows #-}
import FRP.Yampa

data Personality = Hero | Coward

type Pos = (Float,Float)

type AgentIx = Int

data Agent = Agent { personality :: Personality,
                     position :: Pos,
                     friend, enemy :: AgentIx }

type AgentList = [ Agent ]

type AgentSF = SF [Pos] Pos

agent :: Agent -> AgentSF
agent a = loopPre (position a)
                  (proc (ps , oldpos) -> do
                      let fp = ps !! (friend a)
                          ep = ps !! (enemy a)
                        in case personality a of
                              Hero   -> returnA -< (oldpos , oldpos ) -- enter geometry calculation
                              Coward -> returnA -< (oldpos , oldpos )) -- enter geometry calculation

                              
iterateAgents :: [AgentSF] -> [Pos] -> SF () [Pos]
iterateAgents asf ps = pSwitch (\ _ sfs -> map (\ sf -> ( ps , sf)) sfs)
                         asf ((proc (_, newpos) -> returnA -< Event newpos) >>> notYet)
                         (\ sfs newpos -> iterateAgents sfs newpos)
