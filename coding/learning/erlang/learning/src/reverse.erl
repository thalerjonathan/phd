%% @author Jonathan Thaler
%% @doc @todo Add description to reverse.


-module(reverse).

%% ====================================================================
%% API functions
%% ====================================================================
-export([reverse/1]).

reverse( L ) ->
	reverseRec( L, [] ).

%% ====================================================================
%% Internal functions
%% ====================================================================

reverseRec( [], ACC ) ->
	ACC;
reverseRec( [ H | T ], ACC ) ->
	reverseRec( T, [ H | ACC ] ).