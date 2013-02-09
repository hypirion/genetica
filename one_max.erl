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

fitness_fn({phen_to_gen, F, init_vals, [N]}) ->
    fun (Pheno) ->
            Geno = F(Pheno),
            lists:sum(Geno) - 2*abs(N - bit_size(Pheno))
    end.
    
recombination_fn() ->
    todo.

mutation_fn() ->
    todo.
