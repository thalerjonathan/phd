%% @author thjo
%% @doc @todo Add description to foreach.


-module(foreach).

%% ====================================================================
%% API functions
%% ====================================================================
-export([foreach/2]).

foreach( [], Func ) ->
	[];
foreach( [ H | T ], Func ) ->
	[ apply(Func, H) | foreach( T, Func ) ];
foreach( L, Func ) -> 
	foreach( L, Func ).

%% ====================================================================
%% Internal functions
%% ====================================================================
