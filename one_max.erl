-module(one_max).
-import(utils, [atom_to_integer/1, atom_to_float/1]).
-compile(export_all). %% TODO: Remove when finished.

parse_args([Bits, Mutprob, Mutrate | _]) ->
    [atom_to_integer(Bits), atom_to_float(Mutprob), atom_to_float(Mutrate)].

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

crossover(<<N1:1, G1/bitstring>>, <<N2:1, G2/bitstring>>) ->
    {NG1, NG2} = crossover(G1, G2),
    case utils:random_bit() of
        0 -> {<<N1:1, NG1/bitstring>>, <<N2:1, NG2/bitstring>>};
        1 -> {<<N2:1, NG1/bitstring>>, <<N1:1, NG2/bitstring>>}
    end;
crossover(<<>>, <<>>) ->
    {<<>>, <<>>}.

crossover_fn(_) ->
    fun crossover/2.

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
