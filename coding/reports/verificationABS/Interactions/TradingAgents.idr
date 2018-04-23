module TradingAgents

||| This is basically a dependently typed emulation of method calls
||| Which is one of the three interaction mechanisms available in 
||| Agent-Based Simulation
AID : Type
AID = Int

data ATXState = 

||| Note: in the Type-Driven Development book a Process has an 
||| Inf Loop primitve which is omited here because an Agent-Transaction
||| can only have finite number of steps
data AgentTransact : Type ->
                     (msg : Type) ->
                     Type where
  
  -- TODO: a Request ALWAYS has a Response
  Request  : msg -> AgentTransact msg msg
  Response : (msg -> AgentTransact a msg) -> AgentTransact a msg

  -- TODO: need a commit
  -- TODO: need an abort

  Action : IO a -> AgentTransact a msg 
  Pure   : a -> AgentTransact a msg
  (>>=)  : AgentTransact a msg -> 
           (a -> AgentTransact b msg) -> 
           AgentTransact b msg

interface Agent (m : Type -> Type) where
  -- TODO: if we provide a proof that the agent exists, we can omit maybe?
  startInteract : (aid : AID) -> m (Maybe (AgentTransact a msg)) -- TODO: this one needs to start with a Request
  checkInteract : (aid : AID) -> m (Maybe (AgentTransact a msg)) -- TODO: this one needs to start with a Response

data TradingProtocol : Type where
  Offering : Double -> TradingProtocol
  OfferingRefuse : TradingProtocol
  OfferingAccept : TradingProtocol

-- TODO: encode that this one needs to start with a Request
activeATX : AgentTransact () TradingProtocol
activeATX = do
  bla <- Request (Offering 42)
  ?activeATX

-- TODO: encode that this one needs to start with a Response
passiveATX : AgentTransact () TradingProtocol
passiveATX = do
  Respond (\msg => case msg of 
                        (Offering x) => ?bla_1
                        OfferingRefuse => ?bla_2 -- TODO: guarantee that this will not occur in the first step
                        OfferingAccept => ?bla_3 -- TODO: guarantee that this will not occur in the first step
                        )
