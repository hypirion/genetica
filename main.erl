-module(main).
-export([start/1]).
-import(conversions, [bitstr_to_list/1, list_to_bitstr/1]).

start([Name]) ->
    random:seed(now()),
    Geno = (one_max:random_genotype_fn([20]))(),
    io:format("~w~nHello ~s!~n", [Geno, Name]).
genetica_loop(0, Pop, Develop_and_select) ->

generate_random_pop(Popcount, {rand, Rand_gtype, g_to_p, GtoP}) ->
    [GtoP(Genome) || Genome <- utils:repeatedly(Popcount, Rand_gtype)].

genetica_loop(0, _Pop, _Develop_and_select) ->
    done;
genetica_loop(Iters, Pop, Develop_and_select) ->
    Newpop = Develop_and_select(Pop),
    genetica_loop(Iters - 1, Newpop, Develop_and_select).

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
