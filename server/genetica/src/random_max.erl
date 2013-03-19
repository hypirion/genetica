-module(random_max).
-import(genetica_utils, [atom_to_integer/1, atom_to_float/1]).
-export([parse_args/1, random_genotype_fn/1, phenotype_to_genotype_fn/1,
         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
         mutation_fn/1, analyze_fn/1]).

parse_args(Args) ->
    [N | Rest] = one_max:parse_args(Args),
    [(random_genotype_fn([nil, N]))(), N | Rest].

similarity(<<X:1, T1/bitstring>>, <<X:1, T2/bitstring>>) ->
    1 + similarity(T1, T2);
similarity(<<_:1, T1/bitstring>>, <<_:1, T2/bitstring>>) ->
    similarity(T1, T2);
similarity(<<>>, <<>>) ->
    0.

fitness_fn([M | _Args]) ->
    fun ({gtype, Geno}, _) ->
            similarity(M, Geno)
    end.

random_genotype_fn([_M | Args]) -> one_max:random_genotype_fn(Args).
phenotype_to_genotype_fn([_M | Args]) -> one_max:phenotype_to_genotype_fn(Args).
genotype_to_phenotype_fn([_M | Args]) -> one_max:genotype_to_phenotype_fn(Args).
crossover_fn([_M | Args]) -> one_max:crossover_fn(Args).
mutation_fn([_M | Args]) -> one_max:mutation_fn(Args).
analyze_fn(Fitness_fn) -> one_max:analyze_fn(Fitness_fn).
