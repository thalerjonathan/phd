%% @author Jonathan Thaler
%% @doc @todo Add description to agent.


-module(agent).

%% ====================================================================
%% API functions
%% ====================================================================
-export([new/2, makeBids/1, makeAsks/1, sell/3, buy/3, stop/1, reset/1, getState/1, printState/1, agentLoop/1]).

new( I, N ) -> 
	Id = I + 1,
	H = Id / ( N + 1 ),

	Agent = constructState( Id, H, const:initEndowCash(), const:initEndowAsset() ),

	Pid = spawn( agent, agentLoop, [ Agent ] ),

	Pid.

agentLoop( Agent ) ->
	{ Id, H, _, _ } = Agent,

	receive
		{ makeBids, AuctionPid } ->
			BidData = makeBidsInternal( Agent ),
			AuctionPid ! { bids, BidData },
			agentLoop( Agent );

		{ makeAsks, AuctionPid } ->
			AskData = makeAsksInternal( Agent ),
			AuctionPid ! { asks, AskData },
			agentLoop( Agent );

		{ sell, Price, Amount } ->
			NewAgent = sellInternal( Agent, Price, Amount ),
			agentLoop( NewAgent );

		{ buy, Price, Amount } ->
			NewAgent = buyInternal( Agent, Price, Amount ),
			agentLoop( NewAgent );

		{ state, AuctionPid } ->
			AuctionPid ! { state, Agent },
			agentLoop( Agent );

		reset ->
			NewAgent = constructState( Id, H, const:initEndowCash(), const:initEndowAsset() ),
			agentLoop( NewAgent );

		stop ->
			io:format("stopping agent ~w...~n", [ Id ])

	end.

makeBids( Pid ) ->
	Pid ! { makeBids, self() },
	receive
		{ bids, BidData } ->
			BidData
	end.

makeAsks( Pid ) ->
	Pid ! { makeAsks, self() },
	receive
		{ asks, AskData } ->
			AskData
	end.

sell( Pid, Price, Amount ) ->
	Pid ! { sell, Price, Amount }.

buy( Pid, Price, Amount ) ->
	Pid ! { buy, Price, Amount }.

stop( Pid ) ->
	Pid ! stop.

reset( Pid ) ->
	Pid ! reset.

getState( Pid ) ->
	Pid ! { state, self() },
	receive
		{ state, AgentState } ->
			AgentState
	end.

printState( AgentState ) ->
	{ Id, _, Cash, Assets } = AgentState,
	io:format( "Agent ~w with cash = ~.3f, assets = ~.3f ~n", [Id, Cash, Assets]).

%% ====================================================================
%% Internal functions
%% ====================================================================

makeBidsInternal( Agent ) ->
	{ Id, H, Cash, _ } = Agent,

	TradingPrice = erlang:min( Cash, randomRange( const:pD(), assetLimit( H ) ) ),

	if
		TradingPrice > 0 ->
			TradingAmount = const:tradingUnitAsset(),
			{ Id, TradingPrice, TradingAmount };
		true ->
			[]
	end.

makeAsksInternal( Agent ) ->
	{ Id, H, _, Assets } = Agent,

	TradingAmount = erlang:min( Assets, const:tradingUnitAsset() ),

	if
		TradingAmount > 0 ->
			TradingPrice = randomRange( assetLimit( H ), const:pU() ),
			{ Id, TradingPrice, TradingAmount };
		true ->
			[]
	end.

sellInternal( Agent, Price, Amount ) ->
	{ AskerId, AskerH, AskerCash, AskerAssets } = Agent,
	AskerCashAfterTX = AskerCash + Price,
	AskerAssetsAfterTX = AskerAssets - Amount,
	constructState( AskerId, AskerH, AskerCashAfterTX, AskerAssetsAfterTX ).

buyInternal( Agent, Price, Amount ) ->
	{ BidderId, BidderH, BidderCash, BidderAssets } = Agent,
	BidderCashAfterTX = BidderCash - Price,
	BidderAssetsAfterTX = BidderAssets + Amount,
	constructState( BidderId, BidderH, BidderCashAfterTX, BidderAssetsAfterTX ).

randomRange( Lower, Upper ) ->
	Lower + ( rand:uniform() * ( Upper - Lower ) ).

assetLimit( H ) ->
	H * const:pU() + ( 1.0 - H ) * const:pD().

constructState( Id, H, Cash, Assets ) ->
	{ Id, H, Cash, Assets }.