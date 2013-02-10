-module(one_max).
-compile(export_all). %% TODO: Remove when finished.

random_genotype_fn([N | _]) ->
    fun () ->
            conversions:list_to_bitstr(
              utils:repeatedly(N, fun utils:random_bit/0))
    end.

phenotype_to_genotype_fn(_) ->
    fun conversions:list_to_bitstr/1.

genotype_to_phenotype_fn(_) ->
    fun conversions:bitstr_to_list/1.

fitness_fn({init_vals, [N]}) ->
    fun ({indiv, {gtype, Geno}, others, _}) ->
            lists:sum(Geno) - 2*abs(N - bit_size(Geno))
    end.
    
crossover_fn(_) ->
    fun (_G1, _G2) ->
            1
    end.

mutation_fn([P]) ->
    Rec = fun (F, <<N:1,Rest/bitstring>>) ->
                  Rmut = F(F, Rest/bitstring),
                  case random:uniform() =< P of
                      true -> Mutation = utils:random_bit(),
                              <<Mutation:1, Rmut/bitstring>>;
                      false -> <<N:1, Rmut/bitstring>>
                  end;
              (_, <<>>) -> <<>>
          end,
    fun (Geno) ->
            Rec(Rec, Geno)
    end.
