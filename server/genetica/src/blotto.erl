-module(blotto).
-import(utils, [atom_to_integer/1, atom_to_float/1]).
-export([parse_args/1, random_genotype_fn/1, phenotype_to_genotype_fn/1,
         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
         mutation_fn/1, analyze_fn/1]).
-define(BIT_PER_BATTLE, 10).

parse_args([B, Rf, Lf, Mutprob, Mutrate, Crossprob | _]) ->
    [atom_to_integer(B), atom_to_float(Rf), atom_to_float(Lf),
     atom_to_float(Mutprob), atom_to_float(Mutrate), atom_to_float(Crossprob)].

random_genotype_fn([B | _]) ->
    fun () ->
            conversions:list_to_bitstr(
              utils:repeatedly(B*?BIT_PER_BATTLE, fun utils:random_bit/0))
    end.

count_bits(<<N:1,Rest/bitstring>>, Sum) ->
    count_bits(Rest, Sum + N);
count_bits(<<>>, Sum) ->
    Sum.

unzip_strat(<<Battle:?BIT_PER_BATTLE/bitstring, R/bitstring>>, List) ->
    unzip_strat(R, [count_bits(Battle, 0) | List]);
unzip_strat(<<>>, List) ->
    Sum = lists:sum(List),
    if Sum =:= 0 -> Len = length(List),
                    lists:duplicate(Len, 1/Len);
       true -> [X/Sum || X <- List]
    end.

genotype_to_phenotype_fn(_) ->
    fun (Geno) ->
            {strat, unzip_strat(Geno, []), gtype, Geno}
    end.

phenotype_to_genotype_fn(_) ->
    fun ({strat, _, gtype, Geno}) ->
            Geno
    end.

war_points(N) when N < 0 -> 0;
war_points(N) when N > 0 -> 2;
war_points(0) -> 1.

fitness_fn([B, Rf, Lf | _]) ->
    F = fun (_, _, _, _, _, _, _, Win_count, 0) ->
                war_points(Win_count);
            (F,[H1 | T1], R1, S1, [H2 | T2], R2, S2, Win_count, Battles_left) ->
                Troop1 = H1 + R1,     Troop2 = H2 + R2,
                RealH1 = Troop1 * S1, RealH2 = Troop2 * S2,
                BL = Battles_left - 1,
                if RealH1 > RealH2 ->
                        Rem_sold = Rf * max(Troop1 - Troop2, 0),
                        %% May still win with less soldiers
                        F(F, T1, R1 + Rem_sold/max(BL, 1), S1,
                          T2, R2, S2 - Lf,
                          Win_count + 1, BL);
                   RealH1 < RealH2 ->
                        Rem_sold = Rf * max(Troop2 - Troop1, 0),
                        F(F, T1, R1, S1 - Lf,
                          T2, R2 + Rem_sold/max(BL, 1), S2,
                          Win_count - 1, BL);
                   true ->
                        F(F, T1, R1, S1,
                          T2, R2, S2,
                          Win_count, BL)
                end
        end,
    fun (Phenotype, Others) ->
            {strat, S1, gtype, _} = Phenotype,
            Wins = [F(F, S1, 0, 1, S2, 0, 1, 0, B) ||
                       {strat, S2, gtype, _} <- Others],
            lists:sum(Wins) - 1
    end.

crossover_fn([_B, _Rf, _Lf, _Mutprob, _Mutrate, P | _]) ->
    one_max:crossover_fn([nil, nil, nil, 1, P]).

mutation_fn([_B, _Rf, _Lf, Mutprob, Mutrate | _]) ->
    one_max:mutation_fn([nil, Mutprob, Mutrate]).

strategy_entropy({strat, S, gtype, _}) ->
    L2 = math:log(2),
    lists:sum([-P*math:log(P)/L2 || P <- S, P >= 0.01]).

analyze_fn(Fitness_fn) ->
    fun (Pop) ->
            Fits = Fitness_fn(Pop),
            {_, {strat, S, gtype, _}} = lists:max(lists:zip(Fits, Pop)),
            Floats = [utils:std_dev(Fits), lists:max(Fits), lists:min(Fits),
                      utils:avg(lists:map(fun strategy_entropy/1, Pop)), S],
            io:format("~w ~w ~w ~w ~w~n", Floats)
    end.
