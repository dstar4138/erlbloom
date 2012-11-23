%% bf.hrl
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

%%
%% The BloomFilter object, used to pass around into the erlbloom 
%%	library. This is the heavy version of the BF object. To lighten the
%%  load, use the parallel functions which will give you a parallel version
%%  of this object.
-record(bf, {
	% The bloom-filter's bit-vector.
	m = <<>> :: binary(),
	
	% The number of bits in the field.
	n = 64   :: integer(),
	
	% The number of hash functions to run over each element.
	k = 3    :: integer(),
	
	% Hash Function references
	h1 		 :: fun(),
	h2 		 :: fun()
}).
-type bf() :: #bf{}.


%%
%% The parallel version of the BloomFilter object. It mainly just keeps
%% a reference to the server that is keeping track of this filter, so 
%% that you can distribute the load.
-record( pbf, {
	master :: pid()
}).
-type pbf() :: #pbf{}.


%% The type of a bloom filter can be a regular or parallel version.
-type bloomfilter() :: bf() | pbf().
