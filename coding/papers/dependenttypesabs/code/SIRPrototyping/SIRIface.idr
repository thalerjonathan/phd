module SIRIface 

import Control.ST

data SIRState
  = Susceptible
  | Infected
  | Recovered

-------------------------------------------------------------------------------
-- INTERFACE DEFINITION
-------------------------------------------------------------------------------
interface SIRAgent (m : Type -> Type) where
  SIRContext : SIRState -> Type

  init      : (s : SIRState) -> ST m Var [add (SIRContext s)]

  -- making contact with others is only possible in the susceptible state
  makeContact : (ctx : Var) -> ST m () [ctx ::: SIRContext Susceptible]

  -- TODO: getting infected is only possible in the susceptible state
  -- TODO: recovering is only possible in the infected state and occurs after a finite number of time-steps

-------------------------------------------------------------------------------
-- INTERFACE IMPLEMENTATION: IO 
-------------------------------------------------------------------------------
-- TODO: for now IO implementation suffices, lets do a ST implementation later
SIRAgent IO where
  SIRContext x = ?SIRAgent_rhs_1
  init s = ?SIRAgent_rhs_2
  makeContact ctx = ?SIRAgent_rhs_3
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- AGENT IMPLEMENTATION 
-------------------------------------------------------------------------------
sirAgent : (SIRAgent m) => SIRState -> ST m () []
sirAgent Susceptible = ?sirAgent_rhs_1
sirAgent Infected    = ?sirAgent_rhs_2
sirAgent Recovered   = ?sirAgent_rhs_3

susceptible :  (SIRAgent m) 
            => (ctx : Var) 
            -> ST m SIRState [ctx ::: SIRContext {m} Susceptible]
susceptible ctx = pure Susceptible

infected : (SIRAgent m) 
         => (ctx : Var) 
         -> ST m SIRState [ctx ::: SIRContext {m} Infected]

recovered : (SIRAgent m) 
          => (ctx : Var) 
          -> ST m SIRState [ctx ::: SIRContext {m} Recovered]

-------------------------------------------------------------------------------

namespace Main
  main : IO ()
  main = do
    let s0 = Susceptible
    run $ sirAgent s0