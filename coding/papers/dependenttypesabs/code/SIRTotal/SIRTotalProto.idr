module SIRTotalProto

import Data.Vect

-- The idea is to implement a total agent-based SIR simulation 
-- The dynamics of the system-dynamics SIR model are in equilibrium
-- (won't change anymore) when the infected stock is 0. This can 
-- (probably) be shown formally but intuitionistic it is clear because
-- only infected agents can lead to infections of susceptible agents
-- which then make the transition to recovered after having gone
-- through the infection phase.
-- Thus an agent-based implementation of the SIR simulation has to
-- terminate if it is implemented correctly because all infected 
-- agents will recover after a finite number of steps after then 
-- the dyanimcs will be in equilibrium.
-- Thus we need to 'tell' the type-checker the following:
-- 1) no more infected agents is the termination criterion
-- 2) all infected agents will recover after a finite number of time
-- => the simulation will eventually run out of infected agents
-- But when we look at the SIR+S model we have the same termination
-- criterion, but we cannot guarantee that it will run out of infected 
-- => we need additional criteria
-- 4) infected agents are 'generated' by susceptible agents
-- 5) susceptible agents are NOT INCREASING (e.g. recovered agents 
--    do NOT turn back into susceptibles)
-- TODO: can we adopt our solution (if we find it), into a SIRS
-- implementation? this should then break totality. also how difficult
-- is it?

data SIRState 
  = Susceptible
  | Infected
  | Recovered

data SIRAgent : SIRState -> Type where
  MkSIRAgent : SIRAgent s

--------------------------------------------------------------------------------
-- SIR Properties (Propositions)
--------------------------------------------------------------------------------
||| the number of agents stays constant in SIR, this means
||| no agents are created / destroyed during simulation, 
||| they only might change their state 
sirAgentNumberConstant : Vect s (SIRAgent Susceptible) -> 
                         Vect i (SIRAgent Infected) ->
                         Vect r (SIRAgent Recovered) -> 
                         Vect (s + i + r) (SIRAgent st)

||| the number of susceptibles, infected and recovered 
||| might change in each step but the sum will be the 
||| same as before
sirStep : Vect s (SIRAgent Susceptible) -> 
          Vect i (SIRAgent Infected) ->
          Vect r (SIRAgent Recovered) -> 
          (Vect s' (SIRAgent Susceptible), Vect i' (SIRAgent Infected), Vect r' (SIRAgent Recovered), (s'+i'+r') = (s+i+r))