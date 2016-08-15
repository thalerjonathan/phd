%% @author thjo
%% @doc @todo Add description to client.


-module(client).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, echo/1, clientLoop/0]).

start() ->
	spawn( client, clientLoop, [] ).

echo( ServerPid ) -> 
	ServerPid ! { self(), hello }.

clientLoop() ->
	receive
		{ From, Msg } ->
			io:format( "Server replied~n", [] ),
			clientLoop();
		stop ->
			true
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
