-module(selection_mechanisms).
-export([roulette_wheel_fn/1]).

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
            (Lower < V) and (V =< Upper)
    end.

%% Returns a zero-arity function returning the individual who won a single
%% roulette run. Multiple runs may return the same individual.
roulette_wheel_fn(Plist) ->
    {plist, Slotted, total, N} = assign_slots(Plist),
    fun () ->
            Val = random:uniform(N),
            {indiv, I,
             fitness, _,
             slots, _} = utils:ffilter(Slotted, in_interval_fn(Val)),
            I
    end.

sigma_scale(Plist) ->
    Fitnesses = lists:map(fun ({indiv, _, fitness, F}) -> F end, Plist),
    Avg = utils:avg(Fitnesses),
    Stddev = utils:std_dev(Fitnesses, Avg),
    Scalefn = fun (i) -> 1 + (i - Avg)/(2 * Stddev) end,
    lists:keymap(Scalefn, 4, Plist).
