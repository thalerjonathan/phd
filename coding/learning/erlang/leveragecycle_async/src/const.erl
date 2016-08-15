%%%-------------------------------------------------------------------
%%% @author jonathan
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Feb 2016 16:02
%%%-------------------------------------------------------------------
-module(const).
-author("jonathan").

%% API
-export([numberOfAgents/0, tradingUnitAsset/0, initEndowAsset/0, initEndowCash/0, terminateAfterNoMatches/0,
  pD/0, pU/0, replications/0]).

numberOfAgents() -> 100.

tradingUnitAsset() -> 0.1.

initEndowAsset() -> 1.0.
initEndowCash() -> 1.0.

terminateAfterNoMatches() -> 1000.

replications() -> 1.

pD() -> 0.2.
pU() -> 1.0.