-module(one_max).
-import(utils, [atom_to_integer/1, atom_to_float/1]).
-export([parse_args/1, random_genotype_fn/1, phenotype_to_genotype_fn/1,
         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
         mutation_fn/1]).

parse_args([Bits, Mutprob, Mutrate, What_crossover | _]) ->
    [atom_to_integer(Bits), atom_to_float(Mutprob), atom_to_float(Mutrate),
     atom_to_integer(What_crossover)].

random_genotype_fn([N | _]) ->
    fun () ->
            conversions:list_to_bitstr(
              utils:repeatedly(N, fun utils:random_bit/0))
    end.

phenotype_to_genotype_fn(_) ->
    fun ({gtype, Geno}) ->
            Geno
    end.

genotype_to_phenotype_fn(_) ->
    fun (Geno) ->
            {gtype, Geno}
    end.

count_bits(<<N:1,Rest/bitstring>>, Sum) ->
    count_bits(Rest, Sum + N);
count_bits(<<>>, Sum) ->
    Sum.

fitness_fn([N | _]) ->
    fun ({gtype, Geno}, _) ->
            count_bits(Geno, 0) - 2*abs(N - bit_size(Geno))
    end.

crossover1(<<N1:1, G1/bitstring>>, <<N2:1, G2/bitstring>>) ->
    {NG1, NG2} = crossover1(G1, G2),
    case utils:random_bit() of
        0 -> {<<N1:1, NG1/bitstring>>, <<N2:1, NG2/bitstring>>};
        1 -> {<<N2:1, NG1/bitstring>>, <<N1:1, NG2/bitstring>>}
    end;
crossover1(<<>>, <<>>) ->
    {<<>>, <<>>}.

crossover2(G1, G2) ->
    N = random:uniform(bit_size(G1)),
    <<AH:N, AT/bitstring>> = G1,
    <<BH:N, BT/bitstring>> = G2,
    {<<BH:N, AT/bitstring>>, <<AH:N, BT/bitstring>>}.

crossover_fn([_, _, _, 1 | _]) ->
    fun crossover1/2;
crossover_fn([_, _, _, 2 | _]) ->
    fun crossover2/2.

mutation(Mutrate, <<N:1,Rest/bitstring>>) ->
    Rmut = mutation(Mutrate, Rest),
    case random:uniform() =< Mutrate of
        true -> Mutation = utils:random_bit(),
                <<Mutation:1, Rmut/bitstring>>;
        false -> <<N:1, Rmut/bitstring>>
    end;
mutation(_, <<>>) -> <<>>.

mutation_fn([_, Mutprob, Mutrate | _]) ->
    fun (Geno) ->
            case random:uniform() =< Mutprob of
                true -> mutation(Mutrate, Geno);
                false -> Geno
            end
    end.
