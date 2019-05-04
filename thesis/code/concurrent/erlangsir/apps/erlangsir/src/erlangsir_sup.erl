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
    S = 2000,
    I = 1,
    R = 0,
    Beta = 5,
    Gamma = 0.05,
    Delta = 15,
    TMax = 0, % run until all infected agents have recovered

    simulationKernel:init(S, I, R, Beta, Gamma, Delta, TMax),
    
    % TODO: write CSV file

    {ok, {{one_for_all, 0, 1}, []}}.

%%====================================================================
%% Internal functions
%%====================================================================
