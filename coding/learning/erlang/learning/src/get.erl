%% @author Jonathan Thaler
%% @doc @todo Add description to get.


-module(get).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get/2]).

get(N, []) ->
	[];
get(1, [H|T]) -> 
	H;
get(N, [H|T]) -> 
	get( N - 1, T ).

%% ====================================================================
%% Internal functions
%% ====================================================================
