%%%-------------------------------------------------------------------
%% @doc erlangsir top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlangsir_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    S0 = 5000,
    I0 = 10,
    R0 = 0,
    Beta = 5,
    Gamma = 0.05,
    Delta = 15,
    TMax = 0, % run until all infected agents have recovered

    io:fwrite("Running simulation with S0 = ~w, I0 = ~w, R0 = ~w; beta = ~w, gamma = ~w, delta = ~w ... ~n",
      [S0, I0, R0, Beta, Gamma, Delta]),

    {S,I,R, Dyns} = simulationKernel:init(S0, I0, R0, Beta, Gamma, Delta, TMax),
    
    io:fwrite("Simulation finished after ~w steps with final S = ~w, I = ~w, R = ~w ~n",
      [length(Dyns), S, I, R]),

    {ok,Hdl} = file:open("sir.csv", [write]),
    io:format(Hdl, "~s~n", ["T, S, I, R"]),
    lists:map(fun({T,Ss,Is,Rs}) -> 
      io:format(Hdl, "~w, ~w, ~w, ~w ~n", [T, Ss, Is, Rs]) end, Dyns),
    file:close(Hdl),

    io:fwrite("Dynamics written to 'sir.csv' ~n"),

    {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
