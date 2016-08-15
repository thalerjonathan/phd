%% @author thjo
%% @doc @todo Add description to server.


-module(server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, serverLoop/0]).

start() ->
	spawn( server, serverLoop, [] ).

serverLoop() ->
	receive
		{ClientPid, Msg} ->
			ClientPid ! Msg,
			serverLoop();
		stop ->
			true
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

