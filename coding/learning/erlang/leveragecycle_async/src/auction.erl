%% @author Jonathan Thaler
%% @doc @todo Add description to auction.

-module(auction).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	N = const:replications(),

	%% spawning agent-processes
	Agents = createAgents( const:numberOfAgents(), 0 ),

	CummulativeAgentStates = performReplication( Agents, N, [] ),

	%% stop agent-processes
	lists:foreach( fun( Pid ) ->
		agent:stop( Pid )
	end, Agents ),

	MeanAgentStates = lists:map( fun( State ) ->
			{ Id, H, Cash, Assets } = State,
			{ Id, H, Cash / N, Assets / N }
		end, CummulativeAgentStates ),

	lists:foreach( fun( State ) ->
			agent:printState( State )
		end, MeanAgentStates ).

%% ====================================================================
%% Internal functions
%% ====================================================================

performReplication( _, 0, CummulativeAgentStates ) ->
	CummulativeAgentStates;
performReplication( Agents, N, CummulativeAgentStates ) ->
	io:format("Running replication ~w ... ", [ const:replications() - N + 1 ]),

	%% perform auction until criterion is met
	FinalAgentStates = performRound( Agents, 1, const:terminateAfterNoMatches() ),

	%% reset agents
	lists:foreach( fun( Pid ) ->
		agent:reset( Pid )
	end, Agents ),

	io:format("finished~n"),

	if
		CummulativeAgentStates == [] ->
			performReplication( Agents, N - 1, FinalAgentStates );

		true ->
			ZippedAgentStates = lists:zip( FinalAgentStates, CummulativeAgentStates ),

			NewCummulatedAgentStates = lists:map( fun( Z ) ->
				{ RAgent, CAgent } = Z,

				{ Id, H, RCash, RAssets } = RAgent,
				{ Id, H, CCash, CAssets } = CAgent,

				{ Id, H, RCash + CCash, RAssets + CAssets }
			end, ZippedAgentStates ),

			performReplication( Agents, N - 1, NewCummulatedAgentStates )
	end.

performRound( Agents, _, 0 ) ->
	lists:map( fun( Pid ) ->
		agent:getState( Pid )
	end, Agents );
performRound( Agents, Round, NoMatchCountdown ) ->
	%% TODO: implement 500 sweeps: in each sweep agents are allowed to improve their bids/asks
	%% if no match after 500 sweeps, then no match

	ShuffledAgents = utils:shuffle( Agents ),

	AllAsks = lists:map( fun( A ) ->
			agent:makeAsks( A )
		end, ShuffledAgents ),

	AllBids = lists:map( fun( A ) ->
		agent:makeBids( A )
	end, ShuffledAgents ),

	BestAsk = findBest( AllAsks, [], fun ( Ask, BestAsk ) ->
		{ _, Value, _ } = Ask,
		{ _, BestValue, _ } = BestAsk,

		Value < BestValue
	end ),

	BestBid = findBest( AllBids, [], fun ( Bid, BestBid ) ->
		{ _, Value, _ } = Bid,
		{ _, BestValue, _ } = BestBid,

		Value > BestValue
	end ),

	%% if no bestAsk or no bestBid then leave without match
	%% can hardly happen but
	if
		BestAsk == [] orelse BestBid == [] ->
			MatchStatus = nomatch,
			MatchData = [];
		true ->
				{ MatchStatus, MatchData } = findMatch( AllAsks, AllBids, BestAsk, BestBid )
	end,

	if
		MatchStatus == nomatch ->
			NewNoMatchCountdown = NoMatchCountdown - 1;

		MatchStatus == match ->
			NewNoMatchCountdown = const:terminateAfterNoMatches(),
			executeMatch( MatchData, Agents )
	end,

	performRound( Agents, Round, NewNoMatchCountdown ).

findBest( [], Best, _ ) ->
	Best;
findBest( [ [] | Tail ], [], BestFun ) ->
	findBest( Tail, [], BestFun );
findBest( [ [] | Tail ], Best, BestFun ) ->
	findBest( Tail, Best, BestFun );
findBest( [ Elem | Tail ], [], BestFun ) ->
	findBest( Tail, Elem, BestFun );
findBest( [ Elem | Tail ], Best, BestFun ) ->
	IsBetter = BestFun( Elem, Best ),

	if
		IsBetter == true ->
			NewBest = Elem;
		true ->
			NewBest = Best
	end,

	findBest( Tail, NewBest, BestFun ).

findMatch( [], [], _, _ ) ->
	{ nomatch, {} };
findMatch( Asks, Bids, BestAsk, BestBid ) ->

	CheckForAskFirst = rand:uniform() >= 0.5,

	AsksHead = hd( Asks ),
	AsksTail = tl( Asks ),

	BidsHead = hd( Bids ),
	BidsTail = tl( Bids ),

	Match = checkMatch( AsksHead, BidsHead, BestAsk, BestBid, CheckForAskFirst, true ),

	if
		Match =/= [] ->
			Match;
		true ->
			findMatch( AsksTail, BidsTail, BestAsk, BestBid )
	end.

checkMatch( Ask, Bid, BestAsk, BestBid, CheckForAskFirst, FirstCheck ) ->
	if
		CheckForAskFirst ->
			Match = checkPrice( Ask, BestBid );
		true ->
			Match = checkPrice( BestAsk, Bid )
	end,

	if
		Match == [] andalso FirstCheck ->
			checkMatch( Ask, Bid, BestAsk, BestBid, not CheckForAskFirst, false );
		Match == [] andalso FirstCheck == false ->
			[];
		true ->
			Match
	end.

checkPrice( [], _ ) ->
	[];
checkPrice( Ask, Bid ) ->
	{ AskId, AskPrice, _ } = Ask,
	{ BidId, BidPrice, _ } = Bid,

	if
		BidPrice >= AskPrice andalso AskId /= BidId ->
			{ match, { Ask, Bid } };
		true ->
			[]
	end.

executeMatch( MatchData, Agents ) ->
	{ Ask, Bid } = MatchData,

	{ AskerId, AskPrice, AskAmount } = Ask,
	{ BidderId, BidPrice, BidAmount } = Bid,

	AskerPid = lists:nth( AskerId, Agents ),
	BidderPid = lists:nth( BidderId, Agents ),

	TradingAmount = erlang:min( AskAmount, BidAmount ),
	MatchingPrice = ( AskPrice + BidPrice ) / 2.0,
	NormalizedPrice = MatchingPrice * TradingAmount,

	%% asker is asking to sell asset for cash, will get cash and gives away asset
	agent:sell( AskerPid, NormalizedPrice, TradingAmount ),

	%% bidder is bidding to buy asset against cash, will get asset and gives away cash
	agent:buy( BidderPid, NormalizedPrice, TradingAmount ).

createAgents( N, N ) ->
	[];
createAgents( N, I ) ->
	[ agent:new( I, N ) | createAgents( N, I + 1 ) ].
