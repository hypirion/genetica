-module(cognitive).
-import(genetica_utils, [clamp/3, rand_between/2]).
-export([random_genotype_fn/1, phenotype_to_genotype_fn/1,
         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
         mutation_fn/1, analyze_fn/2]).

-record(ptype, {ref, gtype}).
-record(vertex, {name, o = 0, y = 1, tau, sigma, gain}).
-record(edge, {from, to, weight}).

rand_node(Name) ->
    #vertex{name = Name, sigma = rand_between(-10, 0),
            tau = rand_between(1, 2), gain = rand_between(1, 5)}.

random_genotype_fn(_) ->
    fun () ->
            [] ++
                %% edges to a and b, sans sigma
                [rand_between(-5, 5) || _X <- [a, b],
                                        _Y <- [0, 1, 2, 3, 4, a, b]]
                ++ %% edges to c and d, sans sigma
                [rand_between(-5, 5) || _X <- [c, d],
                                        _Y <- [a, b, c, d]]
                ++ %% nodes
                [rand_node(Name) || Name <- [a, b, c, d]]
    end.

phenotype_to_genotype_fn(_) ->
    fun (#ptype{gtype = G}) ->
            G
    end.

gen_fit(_G) ->
    rand_between(0, 100).

refit(#ptype{gtype = G, ref = Ref} = P) ->
    Fitness = gen_fit(G),
    ets:insert(genetica_cognitive_ets, {Ref, Fitness}),
    ok.

setup_ets() ->
    case ets:info(genetica_cognitive_ets) of
        undefined -> ets:new(genetica_cognitive_ets, [set, named_table]);
        _ -> nil
    end,
    ok.


genotype_to_phenotype_fn(_) ->
    setup_ets(),
    %% Generate fitness fn here?
    fun (G) ->
            Ref = erlang:make_ref(),
            P = #ptype{gtype = G, ref = Ref},
            refit(P),
            P
    end.

fitness_fn(_) ->
    fun (#ptype{ref = Ref}, _) ->
            [{Ref, F}] = ets:lookup(genetica_cognitive_ets, Ref),
            F
    end.

crossover_fn(_) ->
    fun (G1, G2) ->
            G1
    end.

mutation_fn(_) ->
    fun (Geno) ->
            Geno
    end.

analyze_fn(Sock, Fitness_fn) ->
    fun (Pop) ->
            Fits = Fitness_fn(Pop),
            {_, #ptype{gtype = Best}} = lists:max(lists:zip(Fits, Pop)),
            Floats = [genetica_utils:avg(Fits), genetica_utils:std_dev(Fits),
                      lists:max(Fits), lists:min(Fits), Best],
            gen_tcp:send(Sock, io_lib:fwrite("~w~n", [Floats])),
            ets:delete_all_objects(genetica_cognitive_ets),
            %% ^ hack, flip over to gen_server for next task.
            lists:foreach(fun refit/1, Pop),
            ok
    end.
