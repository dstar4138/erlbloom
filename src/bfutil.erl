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
-export([ceil/1,mask/3,maskloop/2,bits/1,mod/2]).

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
maskloop({Block,Bit}, M) ->
	try mask(binary:at(M, Block) bor Bit, Block, M)
	catch _:_ -> M end. 
				
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