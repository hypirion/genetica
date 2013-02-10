-module(main).
-export([start/1]).
-import(conversions, [bitstr_to_list/1, list_to_bitstr/1]).

start([Name]) ->
    random:seed(now()),
    Geno = (one_max:random_genotype_fn([20]))(),
    io:format("~w~nHello ~s!~n", [Geno, Name]).

fetch_fns(Module, Opts) ->
    Rand_gtype = Module:random_genotype_fn(Opts),
    PtoG = Module:phenotype_to_genotype_fn(Opts),
    GtoP = Module:genotype_to_phenotype_fn(Opts),
    Fitness = Module:fitness_fn(Opts),
    Crossfn = Module:crossover_fn(Opts),
    Mutfn = Module:mutation_fn(Opts),
    {rand, Rand_gtype, p_to_g, PtoG, g_to_p, GtoP,
     fitness, Fitness, cross, Crossfn, mut, Mutfn}.


%% (1) choose a genetic representation
%% (2) build a population
%% (3) design a fitness function
%% (4) choose a selection operator
%% (5) choose a recombination operator
%% (6) choose a mutation operator
%% (7) devise a data analysis procedure.
