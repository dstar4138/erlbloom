erlbloom
========

Erlang Bloom-Filter with support for parallel lookups and additions.

Made on a whim when I should have been studying for finals. Haven't 
really tested it fully. If you find bugs, I'll be happy to merge fixes!

### How To Use? ###

#### Simple usage: ####

	> BF = erlbloom:create(10000,12). % New bloom filter with 10000 bits and 12 hashes
	> BFa = erlbloom:add(BF, <<"hello">>). % Add an element
	> erlbloom:test(BFa, <<"hello">>). % Test if element is present
	true
	
#### Custom objects: ####

erlbloom can handle more than just binaries, and can handle whatever you throw 
at it. It only asks that you provide a function that will hash it and return a 
binary.

	> Fun1 = ... % A hash function
	> Fun2 = ... % Another hash function
	> BF = erlbloom:create(M,K,[{h1,Fun1},{h2,Fun2}]).
	
Note that erlbloom takes advantage of crypto trick and just requires two hash 
function to implement K hashes.

#### Parallel Bloom Filters: ####

erlbloom has a fairly simplistic concurrency method. Essentially just tell it 
how many "threads" you want it to have and it will spawn that many "testers". 
You can then ask the network of testers if an element exists and each tester 
will check a subrange of the bloom filter. Adding an element just percolates 
the changes out to all testers.

	> PBF = erlbloom:create(M,K,[{threads,8}]). 
	> erlbloom:add(PBF, <<"hello">>).
	> erlbloom:test(PBF, <<"hello">>).
	true
	> erlbloom:stop(PBF).
	ok

Note you do not have to re-bind the results of adding new elements as a 
separate server will maintain that. Also note that because of this, you will 
need to `stop` the parallel bloom filter after finishing with it.

