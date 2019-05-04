-module(agent).

-export([newAgent/5, initPending/5]).

newAgent(SIRState, Beta, Gamma, Delta, Sim) ->
  spawn(?MODULE, initPending, [SIRState, Beta, Gamma, Delta, Sim]).

initPending(SIRState, Beta, Gamma, Delta, Sim) ->
  io:fwrite("agent created, awaiting initialisation... ~n"),
  receive
    {agents, Agents} ->
      case SIRState of
        susceptible ->
          io:fwrite("initialising susceptible agent ~n"),
          susceptible(Agents, Beta, Gamma, Delta, Sim);
        infected ->
          % TODO: draw recovery time random 
          io:fwrite("initialising infected agent ~n"),
          infected(Delta, Sim);
        recovered ->
          io:fwrite("initialising recovered agent ~n"),
          recovered(Sim)
      end
  end.

susceptible(Agents, Beta, Gamma, Delta, Sim) ->
  receive 
    {tick, T} ->
      io:fwrite("received tick ~w in susceptible agent ~n", [T]),
      makeContact(Agents, Beta, Gamma, Delta, Sim);

    {makecontact, Sender} ->
      io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      Sender ! {replycontact, susceptible},
      susceptible(Agents, Beta, Gamma, Delta, Sim)
  end.

makeContact(Agents, Beta, Gamma, Delta, Sim) ->
  AgentCount = length(Agents),

  RandIdx = lists:map(fun(_) -> rand:uniform(AgentCount) end, lists:seq(1, Beta)),
  RandAgents = lists:map(fun(Idx) -> lists:nth(Idx, Agents) end, RandIdx),

  % send makecontact
  lists:map(fun(Agent) -> Agent ! {makecontact, self()} end, RandAgents),

  awaitReply(Agents, Beta, Gamma, Delta, Sim, 0).

awaitReply(Agents, Beta, Gamma, Delta, Sim, N) ->
  receive
    {replycontact, infected} ->
      io:fwrite("received replycontact infected in susceptible, counting ~w ~n", [N]),
      Infected = rand:uniform() =< Gamma,
      if 
        Infected ->
          io:fwrite("susceptible got infected! ~n"),
          % don't forget to report back to simulation kernel
          Sim ! {tickAck},
          % TODO: draw random illness duration
          infected(Delta, Sim);
        true ->
          if 
            % received all replies but no infections happened, back to susceptible
            N+1 == Beta ->
              % don't forget to report back to simulation kernel
              Sim ! {tickAck},
              susceptible(Agents, Beta, Gamma, Delta, Sim);
            % not received all replies yet and no infections happened so far, keep waiting
            true ->
              awaitReply(Agents, Beta, Gamma, Delta, Sim, N+1)
          end
      end;
    {replycontact, _} ->
      io:fwrite("received replycontact _ in susceptible, counting ~w ~n", [N]),
      if 
        % received all replies but no infections happened, back to susceptible
        N+1 == Beta ->
          % don't forget to report back to simulation kernel
          Sim ! {tickAck},
          susceptible(Agents, Beta, Gamma, Delta, Sim);
        % not received all replies yet and no infections happened so far, keep waiting
        true ->
          awaitReply(Agents, Beta, Gamma, Delta, Sim, N+1)
      end;
      
    {makecontact, Sender} ->
      io:fwrite("received makecontact in susceptible from ~w ~n", [Sender]),
      Sender ! {replycontact, susceptible},
      awaitReply(Agents, Beta, Gamma, Delta, Sim, N)
  end.

infected(RecoveryTime, Sim) ->
  receive 
    {tick, T} ->
      io:fwrite("received tick ~w in infected agent ~n", [T]),
      % TODO: check if have recovered
      Sim ! {tickAck},
      infected(RecoveryTime, Sim);

    {makecontact, Sender} ->
      io:fwrite("received makecontact in infected from ~w ~n", [Sender]),
      Sender ! {replycontact, infected},
      infected(RecoveryTime, Sim)
  end.

recovered(Sim) ->
  receive 
    {tick, T} ->
      io:fwrite("received tick ~w in recovered agent ~n", [T]),
      Sim ! {tickAck},
      recovered(Sim);

    {makecontact, Sender} ->
      io:fwrite("received makecontact in recovered from ~w ~n", [Sender]),
      Sender ! {replycontact, recovered},
      recovered(Sim)
  end.