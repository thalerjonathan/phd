%% @author Jonathan Thaler
%% @doc @todo Add description to utils.


-module(utils).

%% ====================================================================
%% API functions
%% ====================================================================
-export([shuffle/1, replace/3]).

shuffle(List) ->
    Random_list = [{random:uniform(), X} || X <- List],
    [X || {_,X} <- lists:sort(Random_list)].

replace(List, Idx, I ) ->
    N = length( List ),
    Pre = lists:sublist( List, 1, Idx - 1 ),
    Post = lists:sublist( List, Idx + 1, N - Idx ),
    Final = lists:append( Pre, [ I | Post ] ),
    Final.

%% ====================================================================
%% Internal functions
%% ====================================================================
