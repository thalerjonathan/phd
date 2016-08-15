%% @author thjo
%% @doc @todo Add description to buildList.


-module(buildList).

%% ====================================================================
%% API functions
%% ====================================================================
-export([buildList/1]).

buildList( N ) -> 
	buildListRec( N, [] ).

%% ====================================================================
%% Internal functions
%% ====================================================================

buildListRec( 0, ACCUM ) ->
	ACCUM;
buildListRec( N, ACCUM  ) ->
	buildListRec( N - 1, [ N | ACCUM ] ).
