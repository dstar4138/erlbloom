%% erlbloom.erl
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
-module(erlbloom).
-include("bf.hrl").
-export([create/1, create/2, create/3, add/2, test/2, stop/1]).
-export([fasthasha/1, fasthashb/1]).

-type bf_opt()  :: {h1, fun()} | {h2, fun()} | 
                   {threads, integer()} | {phash, boolean()}.
-type bf_opts() :: [ bf_opt() ].

%% --------------------------------------------------------------------
%% Simple Create method, useful for generating a temporary bloom filter
%%     for a smallish set.
%%
%% Func: create/1
%% Params: NumElements - The number of expected elements that will be inserted. 
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec create( integer() ) -> {ok, bloomfilter()} | {error, any()}.
create( NumElements ) -> create( bfutil:ceil(NumElements/math:log(2)*5.0), 12).

%% --------------------------------------------------------------------
%% Direct creation method, will create an exact bloomfilter of a specific size.
%%
%% Func: create/2
%% Params: M - The number of bits in the bit-vector.
%%           K - The number of hashes to use on each element. 
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec create( integer(), integer() ) -> {ok, bloomfilter()} | 
                                        {error, any()}.
create( M, K ) -> create(M, K, [{h1, fun erlbloom:fasthasha/1}, 
                                {h2, fun erlbloom:fasthashb/1}]).

%% --------------------------------------------------------------------
%% Direct creation method, will create an exact bloomfilter of a specific size.
%%
%% Func: create/3
%% Params: M - The number of bits in the bit-vector.
%%         K - The number of hashes to use on each element. 
%%         Opts - A list of options for adjusting bloom filter
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec create( integer(), integer(), bf_opts() ) -> {ok, bloomfilter()} | 
                                                   {error, any()}.
create( M, K, Opts ) when is_integer(M) andalso is_integer(K) andalso M > K ->
    {H1, H2,  T, PH} = parse_opts( Opts ),
    B = bfutil:ceil(M/8) * 8, % Must be multiple of 8 (bit syntax)
    case T of
        0 -> {ok, #bf{m = <<0:B>>, n=B, k = K, h1 = H1, h2 = H2}};
        _ -> bf_server:start(B, K, T, PH, H1, H2)
    end.
    
%% --------------------------------------------------------------------
%% Adds an element to a Bloom Filter.
%%
%% Func: add/2
%% Params: BF -  The bloom-filter returned from a `create' function.
%%         Elem - An element that is hashable by the hash functions given to
%%                erlbloom. If you aren't sure, then pass in a binary.
%% Returns: {ok,  BloomFilter} | {error, Reason}
%% --------------------------------------------------------------------
-spec add( bloomfilter(), any() ) -> {ok, bloomfilter()} | 
                                     {error, any()}.
add(BF, Elem) when is_record(BF, bf) ->
   try
        NewM = lists:foldl(fun bfutil:maskloop/2, BF#bf.m, ghl(BF, Elem)),
        {ok, BF#bf{m=NewM}}
   catch 
        _:Reason -> {error, Reason} 
   end;

add(PBF, Elem) when is_record(PBF, pbf) ->
    case bf_server:add( Elem, PBF ) of 
        ok -> {ok, PBF}; % to keep type-sig the same
        Err -> Err
    end.
    
%% --------------------------------------------------------------------
%% Test whether an element has been added to the Bloom Filter.
%%
%% Func: test/2
%% Params: BF - The bloom filter to check in.
%%         Elem - The element that we are checking for. 
%% Returns: true | false | {error, Reason}
%% --------------------------------------------------------------------
-spec test( bloomfilter, any() ) -> boolean() | {error, any() }.
test(BF,Elem) when is_record(BF, bf) ->
    try
        OldM = BF#bf.m,
        lists:foldl(
            fun(BM, C) -> 
                % Short circut if ever C is false.
                C andalso bfutil:checkmask(BM,OldM) 
            end, true, ghl(BF, Elem) )
    catch
        _:Reason -> {error, Reason}
    end;
    
test(PBF, Elem) when is_record(PBF, pbf) -> 
    bf_server:test( Elem, PBF ).


%% --------------------------------------------------------------------
%% Stops the bloom filter server and testing threads.
%%
%% Func: stop/1
%% Params: PBF - The parallel bloom filter.
%% Returns: ok
%% --------------------------------------------------------------------
-spec stop( bloomfilter() ) -> ok.
stop( PBF ) when is_record(PBF, pbf) -> 
    bf_server:stop( PBF );
stop( _ ) -> ok.


%% ====================================================================
%% Built-in Hash functions
%% ====================================================================

%% Hashes the W(ord) K times with two hash functions and then returns the 
%% proper masks in-order to test if an element is present in the filter or
%% to add it.
ghl(BF, W) ->
    lists:foldl( 
      fun (I,Acc) -> 
            H = g(BF#bf.n, I, BF#bf.h1, BF#bf.h2, W),
            [bfutil:bm(H) | Acc]
      end, [], lists:seq(1,BF#bf.k)).

%% Using only two hash functions, we can create K number of hashes using 
%% the formula: H_i(Word) = H_1(Word) + i*H_2(Word) + i^2 mod K. Where K
%% is the size of the ending hash.
g(N, I, H1, H2, X) -> (H1(X) + I*H2(X) + I*I) rem N.
    
%% Two fast hashes using erlang built in functions.
fasthasha(W) -> erlang:phash2(W).
fasthashb(W) -> erlang:phash2({W,"salt"}).


%% ====================================================================
%% Private functions
%% ====================================================================

parse_opts( Opts ) ->
    Def = {fun erlbloom:fasthasha/1, fun erlbloom:fasthashb/1, 0, false},
    lists:foldl(
      fun({Type,Opt}, {A,B,C,D}) ->
        case Type of
            h1 -> {Opt,B,C,D};
            h2 -> {A,Opt,C,D};
            threads -> {A,B,Opt,D};
            phash -> {A,B,C,Opt}
        end
      end, Def, Opts).
