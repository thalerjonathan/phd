%% @author thjo
%% @doc @todo Add description to length.


-module(length).

%% ====================================================================
%% API functions
%% ====================================================================
-export([length/1]).

length( L )	->
	lengthRec( L, 0 ).

%% ====================================================================
%% Internal functions
%% ====================================================================

lengthRec( [], Len ) ->
	Len;
lengthRec( [ H | T ], Len ) ->
	lengthRec( T, Len + 1 ).