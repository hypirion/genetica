-module(selection_mechanisms).
-export([roulette_wheel_fn/2, tournament_selection_fn/2,
         sigma_scale/2, boltzmann_scale/2, rank_scale/2]).

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
roulette_wheel_fn(Plist, _) ->
    {plist, Slotted, total, N} = assign_slots(Plist),
    fun () ->
            Val = random:uniform()*N,
            {indiv, I,
             fitness, _,
             slots, _} = utils:ffilter(Slotted, in_interval_fn(Val)),
            I
    end.

%% Returns a zero-arity function returning the individual who won a tournament
%% with K individuals. May return the same individual.
tournament_selection_fn(Plist, [K]) ->
    fun () ->
            Randomized = utils:shuffle(Plist),
            Firsts = lists:sublist(Randomized, K),
            lists:last(lists:sort(fun fitness_sort/2, Firsts))
    end.

fitness({indiv, _, fitness, F}) -> F.

fitness_sort({indiv, _, fitness, F1}, {indiv, _, fitness, F2}) ->
                     F1 =< F2.

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
