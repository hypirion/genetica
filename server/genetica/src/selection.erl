-module(selection).
-export([roulette_selection_fn/1, tournament_selection_fn/1, fitness_scale/2,
         sigma_scale/2, boltzmann_scale/2, rank_scale/2, full_replacement_fn/4,
         over_production_fn/4, generational_mixing_fn/4]).

%% Selection stuff

%% Assigns slots to the different phenotypes based on their fitness values
assign_slots(Plist) ->
    assign_slots(Plist, 0, []).

assign_slots([{indiv, I, fitness, F} | T], N, Acc) ->
    With_slots = {indiv, I, fitness, F, slots, [N, N+F]},
    assign_slots(T, N + F, [With_slots | Acc]);
assign_slots([], N, Acc) ->
    {plist, Acc, total, N}.

in_interval_fn(V) ->
    fun ({indiv, _, fitness, _, slots, [Lower, Upper]}) ->
            (Lower =< V) and (V < Upper)
    end.

%% Returns a zero-arity function returning the individual who won a single
%% roulette run. Multiple runs may return the same individual.
roulette_selection_fn(_) ->
    fun (Plist) ->
            {plist, Slotted, total, N} = assign_slots(Plist),
            Val = random:uniform()*N,
            {indiv, I,
             fitness, F,
             slots, _} = genetica_utils:ffilter(Slotted, in_interval_fn(Val)),
            {indiv, I,
             fitness, F}
    end.

%% Returns a zero-arity function returning the individual who won a tournament
%% with K individuals. May return the same individual.
tournament_selection_fn([K, P | _]) ->
    fun (Plist) ->
            Randomized = genetica_utils:shuffle(Plist),
            case random:uniform() =< P of
                true -> hd(Randomized);
                false -> Firsts = lists:sublist(Randomized, K),
                         hd(lists:sort(fun fitness_sort/2, Firsts))
            end
    end.

fitness({indiv, _, fitness, F}) -> F.

fitness_sort({indiv, _, fitness, F1}, {indiv, _, fitness, F2}) ->
    F1 >= F2.

fitness_sort_asc({indiv, _, fitness, F1}, {indiv, _, fitness, F2}) ->
    F1 =< F2.

%% Fitness scaling

%% Do not do any scaling of the fitnesses.
fitness_scale(Plist, _) ->
    Plist.

sigma_scale(Plist, _) ->
    Fitnesses = lists:map(fun fitness/1, Plist),
    Avg = genetica_utils:avg(Fitnesses),
    Stddev = genetica_utils:std_dev(Fitnesses, Avg),
    case Stddev =< 0.1 of
        false -> Scalefn = fun (I) -> 1 + max(0, (I - Avg)/(2 * Stddev)) end;
        true -> Scalefn = fun(_) -> 1 end
    end,
    lists:keymap(Scalefn, 4, Plist).

boltzmann_scale(Plist, [T]) ->
    Efit = lists:map(genetica_utils:comp([fun (F) -> math:exp(F/T) end,
                                 fun fitness/1]), Plist),
    Avg = genetica_utils:avg(Efit),
    [{indiv, I, fitness, F / Avg} ||
        {{indiv, I, fitness, _}, F} <- lists:zip(Plist, Efit)].

rank_scale(Plist, _) ->
    Fitnesses = lists:map(fun fitness/1, Plist),
    Max = lists:max(Fitnesses),
    Min = lists:min(Fitnesses),
    N = length(Fitnesses),
    [{indiv, I, fitness, Min + (Max - Min)*(Index - 1)/(N - 1)} ||
        {{indiv, I, fitness, _}, Index} <-
            lists:zip(lists:sort(fun fitness_sort_asc/2, Plist),
                      lists:seq(1, N))].

%% Selection protocols

pick_best(Amount, Pop, Sel_method) ->
    pick_best(Amount, Pop, Sel_method, []).

pick_best(0, _, _, Acc) ->
    Acc;
pick_best(Amount, Pop, Sel_method, Acc) ->
    New = Sel_method(Pop),
    Newpop = Pop -- [New],
    {indiv, I, fitness, _} = New,
    pick_best(Amount - 1, Newpop, Sel_method, [I | Acc]).

full_replacement_fn(Make_child, _, Add_fitness, [Popsize | _]) ->
    fun (Pop, Scale_args) ->
            FPop = Add_fitness(Pop, Scale_args),
            Make_child_from_pop = fun () -> Make_child(FPop) end,
            genetica_utils:repeatedly(Popsize, Make_child_from_pop)
    end.

over_production_fn(Make_child, Selection_fn, Add_fitness, [Popsize, M | _]) ->
    fun (Pop, Scale_args) ->
            FPop = Add_fitness(Pop, Scale_args),
            Make_child_from_pop = fun () -> Make_child(FPop) end,
            Cpop = genetica_utils:repeatedly(Popsize + M, Make_child_from_pop),
            FCpop = Add_fitness(Cpop, Scale_args),
            pick_best(Popsize, FCpop, Selection_fn)
    end.

generational_mixing_fn(Make_child, Selection_fn, Add_fitness,
                       [Popsize, M | _]) ->
    fun (Pop, Scale_args) ->
            FPop = Add_fitness(Pop, Scale_args),
            Make_child_from_pop = fun () -> Make_child(FPop) end,
            Cpop = genetica_utils:repeatedly(Popsize, Make_child_from_pop),
            FCpop = Add_fitness(Cpop, Scale_args),
            Cbest = pick_best(Popsize - M, FCpop, Selection_fn),
            Pbest = pick_best(M, FPop, Selection_fn),
            Cbest ++ Pbest
    end.
