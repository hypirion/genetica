-module(selection).
-export([roulette_selection_fn/2, tournament_selection_fn/2,
         sigma_scale/2, boltzmann_scale/2, rank_scale/2, full_replacement_fn/3,
         over_production_fn/3, generational_mixing_fn/3]).

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
roulette_selection_fn(Plist, _) ->
    {plist, Slotted, total, N} = assign_slots(Plist),
    fun () ->
            Val = random:uniform()*N,
            {indiv, I,
             fitness, _,
             slots, _} = utils:ffilter(Slotted, in_interval_fn(Val)),
            I
    end.

%% Picks an individual based on the fitness value given through a roulette. The
%% fitness values may have been scaled through sigma-scaling, rank-based scaling
%% or boltzmann scaling.
roulette_selection(Plist, _) ->
    {plist, Slotted, total, N} = assign_slots(Plist),
    Val = random:uniform()*N,
    {indiv, I,
     fitness, _,
     slots, _} = utils:ffilter(Slotted, in_interval_fn(Val)),
    I.

%% Returns a zero-arity function returning the individual who won a tournament
%% with K individuals. May return the same individual.
tournament_selection_fn(Plist, [K | _]) ->
    fun () ->
            Randomized = utils:shuffle(Plist),
            Firsts = lists:sublist(Randomized, K),
            lists:last(lists:sort(fun fitness_sort/2, Firsts))
    end.

tournament_selection(Plist, [K | _]) ->
    Randomized = utils:shuffle(Plist),
    Firsts = lists:sublist(Randomized, K),
    lists:last(lists:sort(fun fitness_sort/2, Firsts)).

fitness({indiv, _, fitness, F}) -> F.

fitness_sort({indiv, _, fitness, F1}, {indiv, _, fitness, F2}) ->
                     F1 =< F2.

%% Fitness scaling

%% Do not do any scaling of the fitnesses.
fitness_scale(Plist, _) ->
    Plist.

sigma_scale(Plist, _) ->
    Fitnesses = lists:map(fun fitness/1, Plist),
    Avg = utils:avg(Fitnesses),
    Stddev = max(utils:std_dev(Fitnesses, Avg), 0.001),
    Scalefn = fun (I) -> 1 + (I - Avg)/(2 * Stddev) end,
    lists:keymap(Scalefn, 4, Plist).

boltzmann_scale(Plist, [T]) ->
    Efit = lists:map(utils:comp([fun (F) -> math:exp(F/T) end,
                                 fun fitness/1]), Plist),
    Avg = utils:avg(Efit),
    [{indiv, I, fitness, F / Avg} ||
        {{indiv, I, fitness, _}, F} <- lists:zip(Plist, Efit)].

rank_scale(Plist, _) ->
    Fitnesses = lists:map(fun fitness/1, Plist),
    Max = lists:max(Fitnesses),
    Min = lists:min(Fitnesses),
    N = length(Fitnesses),
    [{indiv, I, fitness, Min + (Max - Min)*(Index - 1)/(N - 1)} ||
        {{indiv, I, fitness, _}, Index} <-
            lists:zip(lists:sort(fun fitness_sort/2, Plist), lists:seq(1, N))].

%% Selection protocols

pick_best(Amount, Pop, Sel_method) ->
    pick_best(Amount, Pop, Sel_method, []).

pick_best(0, _, _, Acc) ->
    Acc;
pick_best(Amount, Pop, Sel_method, Acc) ->
    New = Sel_method(Pop),
    Newpop = Pop -- [New],
    pick_best(Amount - 1, Newpop, Sel_method, [New | Acc]).

full_replacement_fn(Make_child, _, [Popsize | _]) ->
    fun (Pop) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            utils:repeatedly(Popsize, Make_child_from_pop)
    end.

over_production_fn(Make_child, Selection_fn, [Popsize, M | _]) ->
    fun (Pop) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            Cpop = utils:repeatedly(Popsize + M, Make_child_from_pop),
            pick_best(Popsize, Cpop, Selection_fn)
    end.

generational_mixing_fn(Make_child, Selection_fn, [Popsize, M | _]) ->
    fun (Pop) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            Cpop = utils:repeatedly(Popsize, Make_child_from_pop),
            Cbest = pick_best(Popsize - M, Cpop, Selection_fn),
            Pbest = pick_best(M, Pop, Selection_fn),
            Cbest ++ Pbest
    end.
