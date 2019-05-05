-module(simulationKernel).

-export([init/7, initPending/5, replicate/2]).

init(S, I, R, Beta, Gamma, Delta, TMax) ->
  Pid = spawn(?MODULE, initPending, [self(),S,I,R,TMax]),
  %io:fwrite("Sid: ~p ~n", [Pid]),

  % create states for agents
  SIRStates = simulationKernel:replicate(S,susceptible) ++ 
              simulationKernel:replicate(I,infected) ++ 
              simulationKernel:replicate(R,recovered),

  % create all agents
  Agents = lists:map(fun(SIRState) -> agent:newAgent (SIRState,Beta,Gamma,Delta,Pid) end, SIRStates),

  % send all agent ids to agents
  lists:map(fun(A) -> A ! {agents, Agents} end, Agents),
  
  % start simulation
  Pid ! {go, Agents},

  % wait for simulation to be finished
  receive
    {dynamics, Dyns} ->
      io:fwrite("Simulation finished after ~w steps! ~n", [length(Dyns)]),
      %io:fwrite("Simulation finished with dynamics: ~n ~w ~n ", [Dyns]),
      % NOTE: sort dyns by time-stamp because it is very likely that agents
      % which recover within the same Tick dont report in their recovery-time
      % order as it is floating point
      lists:sort(fun({T1,_,_,_}, {T2,_,_,_}) -> T1 =< T2 end, Dyns)
  end.

initPending(Sid, S, I, R, TMax) ->
  receive
    % receive the go with the agents
    {go, Agents} ->
      io:fwrite("Received Go, starting simulation! ~n"),
      % send initial sentTick message to self
      self() ! {sendTick},
      simKernel(Sid, Agents, S, I, R, 1, TMax, 0, [{0, S, I, R}])
  end.

% in case of 0 infected agents we reach equilibrium an can terminate 
% the simulation
simKernel(Sid, _, _, 0, _, _, _, _, Dyns) ->
  %io:fwrite("No more infected agents, finished! ~n"),
  Sid ! {dynamics, Dyns};
simKernel(Sid, Agents, S, I, R, T, TMax, Ack, Dyns) ->
  receive
    % send next tick to all agents
    {sendTick} ->
      TNew = T + 1,
      if
        % we have reached time-limit
        (TNew > TMax + 1) and (TMax > 0) ->
          %io:fwrite("Finished! ~n"),
          % terminate simulation and report dynamics to supervisor
          Sid ! {dynamics, Dyns};
        true ->
          % send tick to all agents
          lists:map(fun(Agent) -> Agent ! {tick, T} end, Agents),
          % continue, increase time by 1 and await S+I acks, ignoring recovered
          simKernel(Sid, Agents, S, I, R, T+1, TMax, S+I, Dyns)
      end;

    % agent acks the tick
    {tickAck} ->
      AckDown = Ack - 1,
      if
        % received all acks
        % ignore Recovered, they don't care about time, stay Recovered FOREVER
        AckDown == 0 ->
          %io:fwrite("All agents tickAck, next tick! ~n"),
          % next tick
          self() ! {sendTick},
          % continue
          simKernel(Sid, Agents, S, I, R, T, TMax, 0, Dyns);
        true ->
          % continue with reduced acks
          simKernel(Sid, Agents, S, I, R, T, TMax, AckDown, Dyns)
      end;

    % an agent got infected at T
    {gotinfected} ->
      % one less susceptible
      NewS = S - 1,
      % one more infected
      NewI = I + 1,
      % add state-change to dynamics
      NewDyns = Dyns ++ [{T, NewS, NewI, R}],
      % continue
      simKernel(Sid, Agents, NewS, NewI, R, T, TMax, Ack, NewDyns);

    % an agent has recovered at the given time-stamp
    {hasrecovered, Ts} ->
      % one less infected
      NewI = I - 1,
      % one more recovered
      NewR = R + 1,
      % add state-change to dynamics
      NewDyns = Dyns ++ [{Ts, S, NewI, NewR}],
      % continue
      simKernel(Sid, Agents, S, NewI, NewR, T, TMax, Ack, NewDyns)
  end.

replicate(N,E) ->
  lists:map(fun(_) -> E end, lists:seq(1, N)).
