-module(main).
-export([start/1]).
-import(conversions, [bitstr_to_list/1, list_to_bitstr/1]).

start([Name]) ->
    random:seed(now()),
    Geno = (one_max:random_genotype_fn([20]))(),
    io:format("~w~nHello ~s!~n", [Geno, Name]).


%% (1) choose a genetic representation
%% (2) build a population
%% (3) design a fitness function
%% (4) choose a selection operator
%% (5) choose a recombination operator
%% (6) choose a mutation operator
%% (7) devise a data analysis procedure.
