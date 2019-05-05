-module(agent).

-export([newAgent/5, initPending/5]).

newAgent(SIRState, Beta, Gamma, Delta, Sim) ->
  spawn(?MODULE, initPending, [SIRState, Beta, Gamma, Delta, Sim]).

initPending(SIRState, Beta, Gamma, Delta, Sim) ->
  %io:fwrite("agent created, awaiting initialisation... ~n"),
  receive
    {agents, Agents} ->
      case SIRState of
        susceptible ->
          %io:fwrite("initialising susceptible agent ~n"),
          susceptible(Agents, Beta, Gamma, Delta, Sim);
        infected ->
          % draw recovery time random 
          IllnessDuration = randomExp(1 / Delta),
          %io:fwrite("initialising infected agent with illnessduration of ~w ~n", [IllnessDuration]),
          % IllnessDuration = RecoveryTime because t = 0 here
          infected(IllnessDuration, Sim);
        recovered ->
          %io:fwrite("initialising recovered agent ~n"),
          recovered(Sim)
      end
  end.

susceptible(Agents, Beta, Gamma, Delta, Sim) ->
  receive
    % next tick from the simulation kernel
    {tick, T} ->
      %io:fwrite("received tick ~w in susceptible agent ~n", [T]),
      makeContact(Agents, Beta, Gamma, Delta, Sim, T);

    % makecontact from susceptible agent. Need to handle this message also here
    % because of race conditions: we can't expect all susceptible agents already
    % in makeContact mode when tick was sent by kernel
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      % reply back with the agent state susceptible
      Sender ! {replycontact, susceptible},
      % stay susceptible
      susceptible(Agents, Beta, Gamma, Delta, Sim)
  end.

makeContact(Agents, Beta, Gamma, Delta, Sim, T) ->
  % draw random agents
  AgentCount = length(Agents),
  RandIdx    = lists:map(fun(_) -> rand:uniform(AgentCount) end, lists:seq(1, Beta)),
  RandAgents = lists:map(fun(Idx) -> lists:nth(Idx, Agents) end, RandIdx),
  % send makecontact to all random agents
  lists:map(fun(Agent) -> Agent ! {makecontact, self()} end, RandAgents),
  % switch to awaiting reply behaviour
  awaitReply(Agents, Beta, Gamma, Delta, Sim, T, 0).

awaitReply(Agents, Beta, Gamma, Delta, Sim, T, N) ->
  receive
    % replycontact from infected agent,
    {replycontact, infected} ->
      %io:fwrite("received replycontact infected in susceptible, counting ~w ~n", [N]),
      % check if infection has occurred
      Infected = rand:uniform() =< Gamma,
      if 
        % got infected
        Infected ->
          % infect the agents
          infectAgent(Delta, Sim, T);
        % not infected
        true ->
          % check if all replies have arrived
          checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N)
      end;
    
    % replycontact from susceptible or recovered agent
    {replycontact, _} ->
      %io:fwrite("received replycontact _ in susceptible, counting ~w ~n", [N]),
      % check if all replies have arrived
      checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N);
      
    % makecontact from susceptible agent
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      % reply back with the agent state susceptible
      Sender ! {replycontact, susceptible},
      % keep waiting
      awaitReply(Agents, Beta, Gamma, Delta, Sim, T, N)
  end.

infectAgent(Delta, Sim, T) ->
  %io:fwrite("susceptible got infected! ~n"),
  % inform kernel about infection
  Sim ! {gotinfected},
  % don't forget to report back to simulation kernel 
  Sim ! {tickAck},
  % draw random illness duration
  IllnessDuration = randomExp(1 / Delta),
  % switch into infected behaviour
  infected(T + IllnessDuration, Sim).

checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N) ->
  if
    % received all replies but no infections happened
    N+1 == Beta ->
      % don't forget to report back to simulation kernel
      Sim ! {tickAck},
      % switch back into susceptible behaviour
      susceptible(Agents, Beta, Gamma, Delta, Sim);
    % not received all replies yet and no infections happened so far
    true ->
      % keep waiting for all replies
      awaitReply(Agents, Beta, Gamma, Delta, Sim, T, N+1)
  end.

infected(RecoveryTime, Sim) ->
  receive 
    % next tick from the simulation kernel
    {tick, T} ->
      %io:fwrite("received tick ~w in infected agent ~n", [T]),
      if 
        % time of recovery has come
        T >= RecoveryTime ->
         recoverAgent(Sim, RecoveryTime);
        % not recovered yet
        true ->
          % report back immediately to finish tick
          Sim ! {tickAck},
          % stay infected
          infected(RecoveryTime, Sim)
      end;

    % makecontact from susceptible agent
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in infected from ~w ~n", [Sender]),
      % reply back with the agent state infected
      Sender ! {replycontact, infected},
      % stay infected
      infected(RecoveryTime, Sim)
  end.

recoverAgent(Sim, RecoveryTime) ->
  % report recovery and the time to kernel
  Sim ! {hasrecovered, RecoveryTime},
  % don't forget to report back to simulation kernel to finish this tick
  Sim ! {tickAck},
  % switch to recovered behaviour
  recovered(Sim).

% NOTE: recovered behaviour does not need to reply to tick because no time-
% dependent behaviour, recovered stays in that behaviour forever
recovered(Sim) ->
  receive
    % makecontact from susceptible agent
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in recovered from ~w ~n", [Sender]),
      % reply back with the agent state recovered
      Sender ! {replycontact, recovered},
      % stay recovered
      recovered(Sim)
  end.

randomExp(Lambda) ->
  R = rand:uniform_real(),
  -(math:log(R)) / Lambda.