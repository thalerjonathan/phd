%% @author Jonathan Thaler
%% @doc @todo Add description to remove.


-module(remove).

%% ====================================================================
%% API functions
%% ====================================================================
-export([remove/2]).

remove(N, L) -> 
	removeRec( N, L ).

%% ====================================================================
%% Internal functions
%% ====================================================================

removeRec( N, [] ) ->
	[];
removeRec( N, [ N | T ] ) ->
	removeRec( N, T );
removeRec( N, [ H | T ] ) ->
	[ H | removeRec( N, T ) ].