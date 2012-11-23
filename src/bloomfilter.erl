%% Simplistic Bloom Filter - 
%%
%% TODO: Scale it up!!
%%	A Scalable Bloom-Filter, 
%%	similar to the one presented in the following paper:
%% 	http://gsd.di.uminho.pt/members/cbm/ps/dbloom.pdf
%%
%% Author: Alexander Dean <dstar@csh.rit.edu>
%%
-module(bloomfilter).
-export([fastmake/1,
		 fastmake/2,
		 make/2,
		 make/4,
		 add/2,
		 test/2]).

-export([fasthasha/1,fasthashb/1]).

-export([clean/1, ptest/2, startTesters/1]).
-export([recvr/3]).

-record(bf, {
	m = <<>> :: binary(),
	n = 64 :: integer(),
	k = 3  :: integer(),
	h1 :: fun(),
	h2 :: fun(),
	spid  = null :: [pid()] | null
}).

%% Builds a bloomfilter based on an expected number of elements (and the 
%%  probabilty of false positives). To minimize the effects of collisions
%%  choosing an M and K correctly is key.
%%
fastmake(N) -> fastmake(N, 0.00001).
fastmake(N,P) -> 
	K = ceil(math:log(1/P)),
	M = ceil(N/math:log(2) * math:log10(1/P)),
	make(M,K).
%% Directly select what you want your M and K to be. Useful if you do not 
%%   know the size of the array.
%% Suggestion: 
%%		k = ceil( ln( 1/p ) )          where p is the prob of false positives.
%%		m = ceil( n/ln(2) * log(1/p) ) where n is the size of the set you 
%%									    want to represent.
make(M,K) -> make(M, K, fun bloomfilter:fasthasha/1, fun bloomfilter:fasthashb/1).
make(M,K,H1,H2) when is_integer(M) and is_integer(K) and M > K ->
	B = (ceil(M/8)),
	#bf{m = <<0:B>>, n=M, k = K, h1 = H1, h2 = H2}.

%% Add an element to a bloom filter.
-spec add(#bf{}, binary()) -> #bf{}.
add(BF, Elem) -> 
	BF#bf{m = erlang:list_to_binary( 
				list:foldr( fun({Block,Bit}, M) ->
					try mask(binary:at(M, Block) bxor Bit, Block, M)
					catch _:_ -> M end 
				end,
				BF#bf.m,
			  	ghl(Elem, BF#bf.n, BF#bf.k, BF#bf.h1, BF#bf.h2 )))}.
	
	
%% Test if the element is in the filter, returns boolean yes or no. However
%% remember that 'yes' actually means 'possibly'.
-spec test(#bf{}, binary()) -> boolean().
test(BF, Elem) -> 
	M = BF#bf.m,
	list:foldr( fun({Block,Bit}, C) ->
					C or (mask(Bit,Block,M)==M)
				end,
				false,
				ghl(Elem, BF#bf.n, BF#bf.k, BF#bf.h1, BF#bf.h2 )).

-spec clean( #bf{} ) -> ok.
clean( BF ) ->
	case BF#bf.spid of
		null -> ok;
		Pids -> killall(Pids)
	end.

-spec startTesters( #bf{} ) -> #bf{}.
startTesters( BF ) -> BF. %TODO: write tester thread.
	

-spec ptest( #bf{}, any() ) -> boolean().
ptest(BF, Elem) when BF#bf.spid =/= null ->
	spawn(bloomfilter, recvr, [BF#bf.spid, Elem,self()]),
	receive
		true -> true;
		false -> false;
		_ -> false
	end.

%%% ================================
%%% Parallel Testing 
%%% ================================
recvr( Pids, Elem, Client ) ->
	lists:foldl(fun (P, _) -> P ! {test, Elem, self()} end, ok, Pids),
	sitspin(Client, length(Pids) ).

sitspin( _ , 0 ) -> ok;
sitspin( Client, Count ) ->
	receive 
		{res, V} -> Client ! V ;
		_ -> ok
	end,
	sitspin(Client, Count - 1).

		
%%% ================================
%%% Built-in Hash functions 
%%% ================================

%% Using only two hash functions, we can create K number of hashes using 
%% the formula: H_i(Word) = H_1(Word) + i*H_2(Word) + i^2 mod K. Where K
%% is the size of the ending hash.
g(N, I, H1, H2, X) -> mod( (H1(X) + I*H2(X) + I*I), N ).
	
%% Two small fast hashes based on Fowler–Noll–Vo-(a/b) hash functions.
%% Note that it requires a binary argument of exactly 64 bits.
fasthasha(<<W:64>>) ->
	D = fun binary:decode_unsigned/1,
	binary:list_to_bin(
		lists:foldl( 
		    fun(Oct,H) -> 
				 (H*1099511628211) bxor D(Oct) 
			end, 14695981039346656037,
			binary:bin_to_list(W))).

fasthashb(W) -> 
	D = fun binary:decode_unsigned/1,
	binary:list_to_bin(
		lists:foldl(
		    fun(Oct,H) -> 
				(H bxor D(Oct)) * 1099511628211
			end, 14695981039346656037,
			binary:bin_to_list(W))).

%%% ================================
%%% Utility Functions
%%% ================================

%% Hashes the W(ord) K times with two hash functions and then returns the 
%% proper masks in-order to test if an element is present in the filter or
%% to add it.
ghl(W,N,K,H1,H2) ->
	lists:foldl( 
	  fun (I,Acc) -> 
			H = g(N,I,H1,H2,W),
			[{H / 8, bits(mod(H,8))}||Acc]
	  end, [], lists:seq(1,K)).

%% Simple ceiling function for fastmake function.
ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.	

%% Mask in a change to a bit, inside of a bitstring.
mask( Block, Num, BitString ) ->
	BitString or << 0:Num, Block, 0:(bit_size(BitString)-(Num+1)) >>.

%% Returns a mask for a particular bit, big-endian.
bits( 0 ) -> <<10000000>>;
bits( 1 ) -> <<01000000>>;
bits( 2 ) -> <<00100000>>;
bits( 3 ) -> <<00010000>>;
bits( 4 ) -> <<00001000>>;
bits( 5 ) -> <<00000100>>;
bits( 6 ) -> <<00000010>>;
bits( 7 ) -> <<00000001>>.

%% Erlang doesn't actually have a mod function.
mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

%% Send a shutdown message to all tester threads.
killall([]) -> ok;
killall([P|R]) -> P ! shutdown, killall(R).
