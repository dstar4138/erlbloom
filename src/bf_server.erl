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
-export([start/6, stop/1, add/2, test/2]).

% Private exports
-export([master/1, tester/3, hasher/5, handler/1]).

% The partial bloom filter, it contains a list of separate blocks of the vector.
-record(partialBF, {
    b = [] :: {integer(), binary()} % {Byte Index, Byte}.
}).

% The master server state.
-record(serv_state, {
    b=0 :: non_neg_integer(),  % The size of the bit vector (number of bits).
    k=0 :: non_neg_integer(),  % The number of hash functions used.
    t=0 :: non_neg_integer(),  % Number of threads for hashing and testing.
    ph=false :: boolean(),     % Whether or not to parallize hashing.
    h1=null :: null | fun(),   % Hash functions
    h2=null :: null | fun(),  
    tpids=[] :: [ pid() ],     % Tester Pid()
    hpids=[] :: [ pid() ],     % Hasher Pids()
    latest_error=null :: null | {pid(), term()} 
}).

%% --------------------------------------------------------------------
%% Starts the bloom filter server and testing threads.
%%
%% Func: start/5
%% Params: B - Size of bit vector.
%%         K - Number of hashes to use on each element.
%%         T - The number of threads to spawn for testing purposes.
%%         PH - Parallel Hashing (best if )
%%         H1 - First hash function.
%%         H2 - Second hash function.
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec start(integer(), integer(), integer(), boolean(), fun(), fun()) -> 
            {ok, [pid()]} | {error, any()}.
start(B, K, T, PH, H1, H2) ->
    MasterPid = start_master( #serv_state{b=B,k=K,t=T,ph=PH,h1=H1,h2=H2} ),
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
%%    to all tester threads for them to update their block segments.
%%
%% Func: add/2
%% Params: Elem - The element to add to the parallel bloom filter.
%%         PBF - The parallel bloom filter.
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
%%         PBF  - The parallel bloom filter.
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
%% Private functions
%% ====================================================================

%% Starts the master server after starting the hashing and testing threads.
start_master(#serv_state{b=B,k=K,t=T,ph=PH,h1=H1,h2=H2}=State) ->
    
    %% If a hasher or tester goes down, master needs to know.
    process_flag(trap_exit, true),
    
    %% Generate handler threads, these will hold onto portions of the bitvector.
    TesterPids = lists:foldl(fun(BVList, PidList) ->
            [spawn_link(bf_server, handler, [#partialBF{b=BVList}])|PidList]
                             end, [], create_byte_partitions( T, B div 8 )),
    
    %% Go ahead and generate hasher threads, these will each run some portion 
    %% of the hash space. (i.e., if you said K=100, and T=10, then each hasher 
    %% will run ten separate hashes.)
    HasherPids = lists:foldl(fun(Hashes,PidList)->
            [spawn_link(bf_server, hasher, [Hashes,H1,H2,B,TesterPids])|PidList]
                             end,[], create_hash_partitions(T, K, PH)),
                                               
    %% Start master server to handle messages from user or testers/hashers.                                           
    master(State#serv_state{tpids=TesterPids, hpids=HasherPids}).


%% The master thread which will spam testers/hashers when an addition, test
%% or shutdown message comes in.
master(#serv_state{tpids=Tpids,hpids=Hpids,latest_error=null} = State) ->
    receive
        shutdown -> 
            flash(Tpids, shutdown),
            flash(Hpids, shutdown);
        
        {add, Elem, RetPid} -> 
            flash(Hpids, {add, Elem}),
            safe_send( RetPid, ok ),
            master(State);
        
        {test, Elem, RetPid} -> 
            Tester = spawn(bf_server, tester, [State,RetPid,0]),
            flash(Hpids, {test, Elem, Tester}),
            master(State);
        
        {'EXIT', From, Reason} -> % Crash and buurn! 
            %TODO: should turn bf_server into OTP, it would be easier to provide 
            %  redundancy in the bitvector.
            flash(Tpids, shutdown),
            flash(Hpids, shutdown),
            master(State#serv_state{latest_error={From,Reason}});
        
        _ -> % Should never happen, but added so we can ignore.
            master(State)
    end.

%% Spawned from the master, it collates the results from the tester threads.
tester(#serv_state{t=T},Ret,T) -> 
    safe_send(Ret, true);
tester(State, Ret, Acc) ->
    receive 
        {ret, true}  -> tester(State, Ret, Acc+1);
        {ret, false} -> safe_send(Ret, false);
        _ -> tester(State,Ret,Acc)
    end.

%% The thread that hashes an element and tells the testers the hashed result.
hasher(Hashs, H1, H2, N, Testers) ->
    receive
        {test, Elem, Serv} ->
            lists:foreach(fun(I) ->
                H = (H1(Elem) + I*H2(Elem) + I*I) rem N,
                flash(Testers, {test, bfutil:bm(H), Serv})
            end, Hashs),
            hasher(Hashs,H1,H2,N,Testers);
        shutdown -> ok;
        _ -> hasher(Hashs,H1,H2,N,Testers)
    end.

%% Thread function that handles checking a sub-section of the bit-vector.
handler(#partialBF{b=B} = State) ->
    receive
        shutdown -> ok;
        
        {test, {Block, Mask}, Ret} ->
            case check(B, Block, Mask) of
                true  -> safe_send(Ret, {ret, true});
                false -> safe_send(Ret, {ret, false});
                _ -> ok
            end,
            handler(State);
        
        {add, Block, Bit} ->
            handler(State#partialBF{b=update(B, Block, Bit)});

        _ -> 
            handler(State)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Performs linear search of bitvector chunks to see if block/bit are 1.
check([{Block,Byte}|_], Block, Mask) ->
    (Byte bor Mask) == Byte;
check([{_,_}|R], Block, Bit) -> 
    check(R, Block, Bit);
check([], _, _) -> none.

%% Performs linear search of bitvector for block/bit and masks it to a 1.
update([{Block,Bits}|R], Block, Bit) ->
    [ {Block, Bits bor Bit} | R ];
update([{_,_}=H|R], Block, Bit) ->
    [ H | update(R,Block,Bit) ];
update([],_,_) -> [].

%% Flashes a list of process-ids with a given message
flash( [Pid|Rest], Msg ) ->  
    flash(Rest, safe_send(Pid, Msg));
flash( [], _ ) -> ok.

%% Break up a bit-vector over N number of threads.
splitbv(N, L) -> splitbv(0,N,L,[]).
splitbv(_,_,[],P) -> P;
splitbv(A,N,[H|T],P) ->
    case lists:keyfind(A,2,P) of
        {A,L} -> splitbv( (A+1) rem N, N, T, lists:keyreplace(A, 2, P, [H,L]));
        false -> splitbv( (A+1) rem N, N, T, [{A,[H]}|P] )
    end.

%% Create all the partitions of the bit-vector given the number of threads 
%% and the number of bytes. 
create_byte_partitions(T, B) ->
    splitbv( T, 
      lists:zip( lists:seq(0, B-1), 
                 lists:duplicate(B,<<0>>) 
            )).

%% Create all the partitions of the hash space given the number of threads
%% and the number of hashes.
create_hash_partitions(T, K, PH) ->
    L = lists:seq(1,K),
    if PH -> splitbv( T, L );
       true -> L
    end.

%% Send a message to a given Process Id. If the process is no longer running,
%% Then just ignore it.
safe_send(Pid,Msg) -> try Pid ! Msg catch _:_ -> Msg end.
