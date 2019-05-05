-module(simulationKernel).

-export([init/7, initPending/5, replicate/2]).

init(S, I, R, Beta, Gamma, Delta, TMax) ->
  Pid = spawn(?MODULE, initPending, [self(),S,I,R,TMax]),
  %io:fwrite("Sid: ~p ~n", [Pid]),

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
      io:fwrite("Simulation finished after ~w steps! ~n", [length(Dyns)]),
      %io:fwrite("Simulation finished with dynamics: ~n ~w ~n ", [Dyns]),
      Dyns
  end.

initPending(Sid, S, I, R, TMax) ->
  receive
    {go, Agents} ->
      io:fwrite("Received Go, starting simulation! ~n"),
      self() ! {sendTick},
      simKernel(Sid, Agents, S, I, R, 1, TMax, 0, [{0, S, I, R}])
  end.

% TODO: clean up code
% TODO: do we really need the Acks? in the end we only need to sort Dyns by their
% time occurrence
simKernel(Sid, _, _, 0, _, _, _, _, Dyns) ->
  %io:fwrite("No more infected agents, finished! ~n"),
  % in case of 0 infected agents we reach equilibrium an can terminate 
  % the simulation
  Sid ! {dynamics, Dyns};
simKernel(Sid, Agents, S, I, R, T, TMax, Ack, Dyns) ->
  receive
    {sendTick} ->
      TNew = T + 1,
      if
        (TNew > TMax + 1) and (TMax > 0) ->
          %io:fwrite("Finished! ~n"),
          Sid ! {dynamics, Dyns};
        true ->
          lists:map(fun(Agent) -> Agent ! {tick, T} end, Agents),
          simKernel(Sid, Agents, S, I, R, T+1, TMax, 0, Dyns)
      end;
    {tickAck} ->
      AckUp = Ack + 1,
      if 
        AckUp == S+I+R ->
          %io:fwrite("All agents tickAck, next tick! ~n"),
          self() ! {sendTick},
          simKernel(Sid, Agents, S, I, R, T, TMax, AckUp, Dyns);
        true ->
          simKernel(Sid, Agents, S, I, R, T, TMax, AckUp, Dyns)
      end;
    {gotinfected} ->
      NewS = S - 1,
      NewI = I + 1,
      NewDyns = Dyns ++ [{T, NewS, NewI, R}],
      simKernel(Sid, Agents, NewS, NewI, R, T, TMax, Ack, NewDyns);
    {hasrecovered, Ts} ->
      NewI = I - 1,
      NewR = R + 1,
      NewDyns = Dyns ++ [{Ts, S, NewI, NewR}],
      simKernel(Sid, Agents, S, NewI, NewR, T, TMax, Ack, NewDyns)
  end.

replicate(N,E) ->
  lists:map(fun(_) -> E end, lists:seq(1, N)).
