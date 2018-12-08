data Event = Tick | Op Int | Change
type AgentOutput = Int
type AgentState  = Int

newtype Agent = Agent (Event -> (AgentOutput, Agent))

agent :: Int -> Agent
agent s Tick = (s, agent s)
agent s (Op v) = (s, agent (s + v))
agent s Change = (s, agentChanged s)
  where
    agentChanged :: Int -> Agent
    agentChanged s Tick = (s, agentChanged s)
    agentChanged s (Op v) = (s, agentChanged (s - v))
    agentChanged s Change = (s, agent s)

main :: IO ()
main = do
  let a0 = agent 0
      a1 = agent 42
  
  a0