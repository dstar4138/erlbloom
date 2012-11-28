%% bfutil.erl
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
-module(bfutil).
-export([ceil/1,checkmask/2,maskloop/2,bm/1]).

%% Simple ceiling function for `create' function.
ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.    

%% Mask in a change to a bit, inside of a bitstring.
checkmask( {Block, Mask}, M ) ->
    B = binary:at(M, Block),
    (B bor Mask) == B.
 
%% Given a block, mask and bitstring, update the bitstring with
%% the given mask at the given byte block.
maskloop({Block, Mask}, OldM) ->
    Front = (Block * 8),
    Rest = erlang:max(erlang:bit_size(OldM) - (Front + 8),0),
    <<_a:Front/bits, B:8, _b:Rest/bits>> = OldM,
    io:format("mask({~p,~p},M:~p) = << _a:~p, ~p, _b:~p >>~n",[Block,Mask,erlang:bit_size(OldM),Front,B,Rest]),
    <<_a/binary, (B bor Mask):8, _b/binary>>.

% Takes a hash and finds the Block for which the hash affects, 
% and the Mask for that block.
bm(H) -> {H div 8, 1 bsl (H rem 8)}.
