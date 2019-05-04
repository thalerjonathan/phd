-module(simulationKernel).

-export([init/7, initPending/5, replicate/2]).

init(S, I, R, Beta, Gamma, Delta, TMax) ->
  Pid = spawn(?MODULE, initPending, [self(),S,I,R,TMax]),
  io:fwrite("Sid: ~p ~n", [Pid]),

  SIRStates = simulationKernel:replicate(S,susceptible) ++ 
              simulationKernel:replicate(I,infected) ++ 
              simulationKernel:replicate(R,recovered),

  Agents = lists:map(fun(SIRState) -> agent:newAgent (SIRState,Beta,Gamma,Delta,Pid) end, SIRStates),

  % send all agent ids to agents
  lists:map(fun(A) -> A ! {agents, Agents} end, Agents),
  
  % start simulation
  Pid ! {go, Agents},

  % wait for simulation finished
  receive
    {dynamics, Dyns} ->
      io:fwrite("Simulation finished with dynamics: ~n ~w ~n ", [Dyns]),
      Dyns
  end.

initPending(Sid, S, I, R, TMax) ->
  receive
    {go, Agents} ->
      io:fwrite("Received Go, starting simulation! ~n"),
      self() ! {sendTick},
      simKernel(Sid, Agents, S, I, R, 1, TMax, 0)
  end.

simKernel(Sid, Agents, S, I, R, T, TMax, Ack) ->
  receive
    {sendTick} ->
      TNew = T + 1,
      if 
        TNew > TMax + 1 ->
          io:fwrite("Finished! ~n"),
          Sid ! {dynamics, ["Dynamics"]};
        true ->
          lists:map(fun(Agent) -> Agent ! {tick, T} end, Agents),
          simKernel(Sid, Agents, S, I, R, T+1, TMax, 0)
      end;
    {tickAck} ->
      AckUp = Ack + 1,
      if 
        AckUp == S+I+R ->
          io:fwrite("All agents tickAck, next tick! ~n"),
          self() ! {sendTick},
          simKernel(Sid, Agents, S, I, R, T, TMax, AckUp);
        true ->
          simKernel(Sid, Agents, S, I, R, T, TMax, AckUp)
      end;
    {gotinfected, Ts} ->
      simKernel(Sid, Agents, S-1, I+1, R, T, TMax, Ack);
    {hasrecovered, Ts} ->
      simKernel(Sid, Agents, S, I-1, R+1, T, TMax, Ack)
  end.

replicate(N,E) ->
  lists:map(fun(_) -> E end, lists:seq(1, N)).
