%% @author thjo
%% @doc @todo Add description to 'findMax'.


-module('findMax').

%% ====================================================================
%% API functions
%% ====================================================================
-export([findMax/1]).

findMax( [] ) ->
	nil;
findMax( L ) ->
	findMaxRec( L, hd( L ) ).

%% ====================================================================
%% Internal functions
%% ====================================================================

findMaxRec( [ H | T ], Max ) when H > Max -> 
	findMaxRec( T, H );
findMaxRec( [ H | T ], Max ) -> 
	findMaxRec( T, Max );
findMaxRec( [], Max ) -> 
	Max.