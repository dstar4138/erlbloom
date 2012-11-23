%% bf_server.erl
%%
%% @copyright 2012 Alexander Dean <dstar@csh.rit.edu>
%% @end
%%
%% This library is free software; you can redistribute it and/or
%% modify it under the terms of the GNU Lesser General Public
%% License as published by the Free Software Foundation; either 
%% version 2.1 of the License, or (at your option) any later version.
%% 
%% This library is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library. If not, see <http://www.gnu.org/licenses/>.
-module(bf_server).
-include("bf.hrl").

% Public exports
-export([start/5,stop/1,add/2,test/2]).

% Private exports
-export([start_master/2, master/1, handler/1]).


% The partial bloom filter, it contains a list of separate bytes of the vector.
-record(partialBF, {b=[]}).

% The master server state.
-record(serv_state, {
	b=0,	  % The Size of the bit vector (number of bits)
	k=0,      % The number of hash functions used
	h1=null,  % Hash functions
	h2=null,  
	tpids=[], %Tester Pid()
	hpids=[]  %Hasher Pids()
}).


%% --------------------------------------------------------------------
%% Starts the bloom filter server and testing threads.
%%
%% Func: start/5
%% Params: M - Size of bit vector.
%%		   K - Number of hashes to use on each element.
%%		   N - The number of threads to spawn for testing purposes.
%%		   H1 - First hash function.
%%		   H2 - Second hash function.
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec start(integer(), integer(), integer(), fun(), fun()) -> {ok, [pid()]} |
															  {error, any()}.
start(M, K, N, H1, H2) ->
	State = #serv_state{b=M,k=K,h1=H1,h2=H2},
	MasterPid = spawn(bf_server,start_master,[State, N]),
	{ok, #pbf{master=MasterPid} }.

%% --------------------------------------------------------------------
%% Stops the bloom filter server and testing threads.
%%
%% Func: stop/1
%% Params: PBF - The parallel bloom filter.
%% Returns: ok
%% --------------------------------------------------------------------
-spec stop( pbf() ) -> ok.
stop( PBF ) -> 
	PBF#pbf.master ! shutdown, ok.

%% --------------------------------------------------------------------
%% Adds an element to a Bloom Filter. It does so by sending the hashes 
%%	to all tester threads for them to update their block segments.
%%
%% Func: add/2
%% Params: Elem - The element to add to the parallel bloom filter.
%%		   PBF - The parallel bloom filter.
%% Returns: ok | {error, Reason}
%% --------------------------------------------------------------------
-spec add( term(), pbf() ) -> ok | {error, any()}.
add(Elem, PBF) -> 
	try 
		PBF#pbf.master ! {add, Elem, self()},
		receive Msg -> Msg end
	catch 
		_:_ -> {error, badarg} 
	end.

%% --------------------------------------------------------------------
%% Tests if an element is present in the parallel bloom filter. It does 
%%  so by blasting a message to all tester threads and waits for a reply
%%  from all.
%%
%% Func: test/2
%% Params: Elem - The element to test if it exists.
%%		   PBF  - The parallel bloom filter.
%% Returns: true | false | {error, Reason}
%% --------------------------------------------------------------------
-spec test( term(), pbf() ) -> boolean() | {error, any()}.
test(Elem, PBF) -> 
	try
		PBF#pbf.master ! {test, Elem, self()},
		receive Msg -> Msg end
	catch
		_:_ -> {error, badarg}
	end.
	

%% ====================================================================
%% Internal functions
%% ====================================================================

start_master(#serv_state{b=B,k=K,h1=H1,h2=H2}=State, T) ->
	BV = splitbv(T,lists:zip( lists:seq(0, B-1), lists:duplicate(B,<<0:1>>) )),
	TesterPids = lists:foldl(fun(BVList, PidList) ->
								[spawn(bf_server, handler, [#partialBF{b=BVList}])|PidList]
							 end, [], BV),
	HasherPids = lists:foldl(fun(_,PidList)->
								[spawn(bf_server, hasher, [State])|PidList]
							 end,[], lists:seq(0, T)),
	master(State#serv_state{tpids=TesterPids,hpids=HasherPids}).

master(#serv_state{tpids=TesterPids,hpids=HasherPids} = State) ->
	receive
		shutdown -> 
			flash(TesterPids, shutdown);
		{add, Elem, RetPid} -> 
			flash(HasherPids, {add, Elem}),
			RetPid ! ok,
			master(State);
		{test, Elem, RetPid} -> 
			Tester = spawn(bf_server, tester, [State,RetPid,0]),
			flash(HasherPids, {test, Elem, Tester})
	end.

tester(State, Ret, Acc) ->
	receive 
		{ret, }
	
hasher(Elem, N, H1, H2, Start, End, Pids) ->
	lists:foreach(fun(I) ->
		H = bfutil:mod( (H1(Elem) + I*H2(Elem) + I*I), N ),
		flash(Pids, {test,H/8,bfutils:mod(H,8)})
	end, lists:seq(Start, End)).	

%% Parallel thread function that handles checking a subsection of the bitvector.
handler(#partialBF{b=B} = State) ->
	receive
		shutdown -> ok;
		{test, Block, Bit, Ret} ->
			case check(B, Block, Bit) of
				true -> Ret ! {ret, true};
				false -> Ret ! {ret, false};
				_ -> ok
			end,
			handler(State);
		{add, Block, Bit} ->
			State#partialBF{b=update(B, Block, Bit)};
		_ -> handler(State)
	end.

%%% Internal functions
check([{Block,Bits}|_], Block, Bit) ->
	(Bits bor Bit) == Bits;
check([{_,_}|R], Block, Bit) -> 
	check(R, Block, Bit);
check([], Block, Bit) -> none.

update([{Block,Bits}|R], Block, Bit) ->
	[ {Block, Bits bor Bit} | R ];
update([{_,_}=H|R], Block, Bit) ->
	[ H | update(R,Block,Bit) ];
update([],_,_) -> [].

flash( [Pid|Rest], Msg ) -> Pid ! Msg;
flash( [], _ ) -> ok.

splitbv(N, L) -> splitbv(0,N,L,[]).
splitbv(A,N,[],P) -> P;
splitbv(A,N,[H|T],P) ->
	case lists:keyfind(A,2,P) of
		{A,L} -> splitbv( (A+1) rem N, N, T, lists:keyreplace(A, 2, P, [H,L]));
		false -> splitbv( (A+1) rem N, N, T, [{A,[H]}|P] )
	end.
	
  
