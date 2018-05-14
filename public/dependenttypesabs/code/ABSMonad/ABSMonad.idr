module ABSMonad

-- an agent is an indexed monad over 
-- m:       underlying computation (monad) context
-- ty:      return-type of the command
-- sa_pre:  some inner state s previous of the command
-- sa_post: some inner state s' after the command
-- evt:     an event/interaction protocoll
-- w, h:    2d environment boundary parameters
-- t:       current simulation time-step t
data Agent : (m : Type -> Type) -> 
             (ty : Type) ->
             (sa_pre : Type) -> 
             (sa_post : Type) ->
             (evt : Type) ->
             (w : Nat) -> 
             (h : Nat) -> 
             (t : Nat) -> Type where
  