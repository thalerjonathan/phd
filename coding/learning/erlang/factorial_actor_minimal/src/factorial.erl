%%%-------------------------------------------------------------------
%%% @author jonathan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Jun 2016 16:16
%%%-------------------------------------------------------------------
-module(factorial).
-author("jonathan").

%% API
-export( [ calc/1, factLoop/0, factContLoop/2 ] ).

calc( Val ) ->
  FactPid = spawn( factorial, factLoop, [] ),
  FactPid ! { Val, self() },

  receive
    Value ->
      io:format( "~w! = ~w ~n", [ Val, Value ])
  end.

factLoop() ->
  receive
    { Val, Cust } ->
      if
        Val == 0 ->
          Cust ! 1;

        true ->
          NewCust = spawn( factorial, factContLoop, [ Val, Cust ] ),
          self() ! { ( Val - 1 ), NewCust }
      end,

      factLoop()
  end.

factContLoop( Val, Cust ) ->
  receive
    Arg ->
      Cust ! ( Val * Arg )
  end.