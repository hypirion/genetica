-module(selection_protocols).
-export([full_replacement_fn/2, over_production_fn/2,
         generational_mixing_fn/2]).

pick_best(Amount, Pop, Sel_method) ->
    pick_best(Amount, Pop, Sel_method, []).

pick_best(0, _, _, Acc) ->
    Acc;
pick_best(Amount, Pop, Sel_method, Acc) ->
    New = Sel_method(Pop),
    Newpop = Pop -- [New],
    pick_best(Amount - 1, Newpop, Sel_method, [New | Acc]).

full_replacement_fn(Make_child, _) ->
    fun (Pop, [Popsize | _]) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            utils:repeatedly(Popsize, Make_child_from_pop)
    end.

over_production_fn(Make_child, Selection_fn) ->
    fun (Pop, [Popsize, M | _]) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            Cpop = utils:repeatedly(Popsize + M, Make_child_from_pop),
            pick_best(Popsize, Cpop, Selection_fn)
    end.

generational_mixing_fn(Make_child, Selection_fn) ->
    fun (Pop, [Popsize, M | _]) ->
            Make_child_from_pop = fun () -> Make_child(Pop) end,
            Cpop = utils:repeatedly(Popsize, Make_child_from_pop),
            Cbest = pick_best(Popsize - M, Cpop, Selection_fn),
            Pbest = pick_best(M, Pop, Selection_fn),
            Cbest ++ Pbest
    end.
