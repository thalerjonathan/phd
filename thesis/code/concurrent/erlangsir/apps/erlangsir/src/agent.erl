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
    {tick, T} ->
      %io:fwrite("received tick ~w in susceptible agent ~n", [T]),
      makeContact(Agents, Beta, Gamma, Delta, Sim, T);

    {makecontact, Sender} ->
      %io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      Sender ! {replycontact, susceptible},
      susceptible(Agents, Beta, Gamma, Delta, Sim)
  end.

makeContact(Agents, Beta, Gamma, Delta, Sim, T) ->
  AgentCount = length(Agents),
  % draw random agents
  RandIdx = lists:map(fun(_) -> rand:uniform(AgentCount) end, lists:seq(1, Beta)),
  RandAgents = lists:map(fun(Idx) -> lists:nth(Idx, Agents) end, RandIdx),

  % send makecontact
  lists:map(fun(Agent) -> Agent ! {makecontact, self()} end, RandAgents),
  % switch into awaiting reply behaviour
  awaitReply(Agents, Beta, Gamma, Delta, Sim, T, 0).

awaitReply(Agents, Beta, Gamma, Delta, Sim, T, N) ->
  receive
    {replycontact, infected} ->
      %io:fwrite("received replycontact infected in susceptible, counting ~w ~n", [N]),
      Infected = rand:uniform() =< Gamma,
      if 
        Infected ->
          infectAgent(Delta, Sim, T);
        % not infected
        true ->
          % check if all replies have arrived
          checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N)
      end;

    {replycontact, _} ->
      %io:fwrite("received replycontact _ in susceptible, counting ~w ~n", [N]),
      checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N);
      
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      Sender ! {replycontact, susceptible},
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
  infected(T + IllnessDuration, Sim).

checkAllReplies(Agents, Beta, Gamma, Delta, Sim, T, N) ->
  if
    % recseived all replies but no infections happened, back to susceptible
    N+1 == Beta ->
      % don't forget to report back to simulation kernel
      Sim ! {tickAck},
      susceptible(Agents, Beta, Gamma, Delta, Sim);
    % not received all replies yet and no infections happened so far, keep waiting
    true ->
      awaitReply(Agents, Beta, Gamma, Delta, Sim, T, N+1)
  end.

infected(RecoveryTime, Sim) ->
  receive 
    {tick, T} ->
      %io:fwrite("received tick ~w in infected agent ~n", [T]),
      if 
        % recovered
        T >= RecoveryTime ->
          % report recovery and the time to kernel
          Sim ! {hasrecovered, RecoveryTime},
          % don't forget to report back to simulation kernel
          Sim ! {tickAck},
          % switch to recovered behaviour
          recovered(Sim);
        % not recovered yet
        true ->
          Sim ! {tickAck},
          infected(RecoveryTime, Sim)
      end;

    % reply to makecontact from susceptible agent
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in infected from ~w ~n", [Sender]),
      Sender ! {replycontact, infected},
      infected(RecoveryTime, Sim)
  end.

recovered(Sim) ->
  receive
     % ack the tick to simulation kernel
    {tick, _} ->
      %io:fwrite("received tick ~w in recovered agent ~n", [T]),
      Sim ! {tickAck},
      recovered(Sim);

    % reply to makecontact from susceptible agent
    {makecontact, Sender} ->
      %io:fwrite("received makecontact in recovered from ~w ~n", [Sender]),
      Sender ! {replycontact, recovered},
      recovered(Sim)
  end.

randomExp(Lambda) ->
  R = rand:uniform_real(),
  -(math:log(R)) / Lambda.