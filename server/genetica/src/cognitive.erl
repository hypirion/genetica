-module(cognitive).
-import(genetica_utils, [clamp/3, rand_between/2]).
-export([random_genotype_fn/1, phenotype_to_genotype_fn/1,
         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
         mutation_fn/1, analyze_fn/2]).

-record(ptype, {ref, gtype}).
-record(vertex, {name, tau, sigma, gain}).
-record(vstate, {o = 0, y = 0}).
-record(edge, {from, to}).
-record(block, {size, x, y}).

-define(AREA_HEIGHT, 15).
-define(AREA_WIDTH, 30).
-define(TRACKER_SIZE, 5).
-define(BLOCK_MAX_SIZE, 6).
-define(NOF_BLOCKS, 40).
-define(ETS_INTEGERS, [{{'$1','$2'},[{is_integer,'$1'}],['$2']}]).
-define(ETS_REF_DELETE, [{{'$1','_'},[],[{is_reference,'$1'}]}]).

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

%%% Fitness function stuff
results_to_fitness(Res) ->
    Avoids = [Y || {X, Y} <- Res, X =:= avoid],
    ALen = length(Avoids),
    Captures = [Y || {X, Y} <- Res, X =:= capture],
    CLen = length(Captures),
    Avs = lists:sum(Avoids),
    Capts = lists:sum(Captures),
%    io:format("~p -- ~p~n", [Avs, Capts]),
    Capts + Avs.

gen_fit(G) ->
    Res = simulate_run(G),
    results_to_fitness(Res).

brute_fit(G) ->
    Res = brute_run(G),
    results_to_fitness(Res).

brute_run(G) ->
    Blocks = [#block{size = S, x = X, y = ?AREA_HEIGHT} ||
                 S <- lists:seq(1, ?BLOCK_MAX_SIZE),
                 X <- lists:seq(1, ?AREA_WIDTH)],
    Ets = init_local_ets(G),
    Res = lists:map(fun (B) -> single_brute(Ets, B) end, Blocks),
    ets:delete(Ets),
    Res.

single_brute(Ets, Block) ->
    ets:insert(Ets, Block),
    ets:insert(Ets, {tpos, 0}),
    genetica_utils:repeatedly(?AREA_HEIGHT, fun() -> step(Ets) end),
    sense(Ets),
    Sensed = ets:select(Ets, ?ETS_INTEGERS),
    reset_tracker(Ets),
    case ets:lookup(Ets, block) of
        [#block{size=S, y=0, x=_}] ->
            if S >= ?TRACKER_SIZE ->
                    {avoid, case lists:sum(Sensed) of
                                0 -> 1;
                                ?TRACKER_SIZE -> 0;
                                _ -> 0.1
                            end};
               S < ?TRACKER_SIZE ->
                    {capture, case lists:sum(Sensed) of
                                  S -> 1;
                                  _ -> 0
                              end}
            end
    end.

new_block(Ets) ->
    ets:insert(Ets, #block{size = random:uniform(?BLOCK_MAX_SIZE),
                           x = random:uniform(?AREA_WIDTH),
                           y = ?AREA_HEIGHT}).

%% Whether we can sense if we're at pos At, and the block is at spec. pos.
senses(At, #block{size = S, x = X}) ->
    case At >= ?AREA_WIDTH of
        true -> At_m = At - ?AREA_WIDTH;
        false -> At_m = At
    end,
    Cmp =
        fun(A) when A >= ?AREA_WIDTH ->
                A - ?AREA_WIDTH =:= At_m;
           (A) -> A =:= At_m
        end,
    case lists:any(Cmp, lists:seq(X, X + S - 1)) of %% - 1 to avoid blocksize+1
        true -> 1;
        false -> 0
    end.

sense(Ets) ->
    [Block] = ets:lookup(Ets, block),
    [{tpos, P}] = ets:lookup(Ets, tpos),
    ets:insert(Ets, lists:zip(lists:seq(1, ?TRACKER_SIZE),
                              [senses(At, Block) ||
                                  At <- lists:seq(P,P + ?TRACKER_SIZE - 1)])).

actuate(Ets) ->
    [{c, _, #vstate{o = C}}] = ets:lookup(Ets, c),
    [{d, _, #vstate{o = D}}] = ets:lookup(Ets, d),
    [{tpos, OldPos}] = ets:lookup(Ets, tpos),
    Delta = abs(clamp(round(5*(D-C)), -4, 4)),
    [{delta, X}] = ets:lookup(Ets, delta),
    ets:insert(Ets, {delta, X + Delta}),
    RawPos = OldPos + clamp(round(5*(D-C)), -4, 4),
    if RawPos < 0 ->
            NewPos = RawPos + ?AREA_WIDTH;
       RawPos >= ?AREA_WIDTH ->
            NewPos = RawPos - ?AREA_WIDTH;
       true ->
            NewPos = RawPos
    end,
%    io:format("Old position: ~p, new position: ~p~n", [OldPos, NewPos]),
    ets:insert(Ets, {tpos, NewPos}).

edge_from(X) when X =:= a orelse X =:= b ->
    [a, b] ++ lists:seq(1, ?TRACKER_SIZE);
edge_from(X) when X =:= c orelse X =:= d ->
    [a, b, c, d].

get_output(Ets, X) when is_atom(X) ->
    [{X, _, #vstate{o = O}}] = ets:lookup(Ets, X),
    O;
get_output(Ets, X) when is_integer(X) ->
    [{X, O}] = ets:lookup(Ets, X),
    O.

new_vstate(Ets, X) ->
    Edges = [Edge || F <- edge_from(X),
                     Edge <- ets:lookup(Ets, #edge{to=X, from=F})],
    %% ^ one level flatten
    Sum = lists:sum([case Edge of
                     {#edge{to=X, from=F}, W} ->
                         get_output(Ets, F) * W
                     end || Edge <- Edges]),
    [{X, #vertex{tau=Tau, sigma=Sigma, gain=G, name=X} = V,
         #vstate{o=_OldO, y=OldY}}]
        = ets:lookup(Ets, X),
    DY = (Sum + Sigma - OldY)/Tau,
    Y = OldY + DY,
    O = 1 / (1 + math:exp(-G * Y)),
    ets:insert(Ets, {X, V, #vstate{o=O, y=Y}}),
    ok.

cognitive_step(Ets) ->
    [new_vstate(Ets, Name) || Name <- [a, b, c, d]].

move_block(Ets) ->
    [Block = #block{y=Y}] = ets:lookup(Ets, block),
    ets:insert(Ets, Block#block{y=Y-1}).

step(Ets) ->
    sense(Ets),
    cognitive_step(Ets),
    actuate(Ets),
    move_block(Ets),
%    io:format("~P~n~n", [ets:match_object(Ets, '_'), 100]),
    nil.

reset_tracker(Ets) ->
    ets:insert(Ets,
               [case ets:lookup(Ets, V)
                    of [{X, Y, _}] ->
                        {X, Y, #vstate{}}
                end || V <- [a, b, c, d]]).

block_run(Ets) ->
    new_block(Ets),
    genetica_utils:repeatedly(?AREA_HEIGHT, fun() -> step(Ets) end),
    sense(Ets),
    Sensed = ets:select(Ets, ?ETS_INTEGERS),
    reset_tracker(Ets),
    case ets:lookup(Ets, block) of
        [#block{size=S, y=0, x=_}] ->
            if S >= ?TRACKER_SIZE ->
                    {avoid, case lists:sum(Sensed) of
                                0 -> 1;
                                ?TRACKER_SIZE -> 0;
                                _ -> 0.1
                            end};
               S < ?TRACKER_SIZE ->
                    {capture, case lists:sum(Sensed) of
                                  S -> 1;
                                  _ -> 0
                              end}
            end
    end.

init_local_ets(G) ->
%    io:format("whole:~n~p~n~n", [G]),
    Ets = ets:new(x, [set]),
    ets:insert(Ets, {delta, 0}),
    Firsts = lists:zip([#edge{from=F, to=T} || T <- [a, b],
                                               F <- [1, 2, 3, 4, 5, a, b]],
                       lists:sublist(G, 2*7)),
    ets:insert(Ets, Firsts),
%    io:format("first 14:~n~p~n~n", [Firsts]),
    OtherEdges = lists:zip([#edge{from=F, to=T} || T <- [c, d],
                                                   F <- [a, b, c, d]],
                           lists:sublist(G, 2*7 + 1, 2*4)),
%    io:format("next 8:~n~p~n~n", [OtherEdges]),
    ets:insert(Ets, OtherEdges),
    Vertices = lists:sublist(G, 2*7 + 2*4 + 1, 4),
%    io:format("and finally, vertices:~n~p~n~n", [Vertices]),
    ets:insert(Ets, [{Name, Vertex, #vstate{}} ||
                        Vertex = #vertex{name=Name} <- Vertices]),
    ets:insert(Ets, {tpos, 0}),
    Ets.

simulate_run(G) ->
    Ets = init_local_ets(G),
    Res = genetica_utils:repeatedly(?NOF_BLOCKS, fun () -> block_run(Ets) end),
    ets:delete(Ets),
    Res.

refit(G) ->
    [{simtype, S}] = ets:lookup(genetica_cognitive_ets, simtype),
    refit(S, G).

refit(random, #ptype{gtype = G, ref = Ref}) ->
    Fitness = gen_fit(G),
    {Ref, Fitness};
refit(brute, #ptype{gtype = G, ref = Ref}) ->
    case ets:lookup(genetica_cognitive_ets, Ref) of
        [] ->
            Fitness = brute_fit(G),
            {Ref, Fitness};
        [{Ref, Fitness}] ->
            {Ref, Fitness}
    end.

init_global_ets(SimType) ->
    case ets:info(genetica_cognitive_ets) of
        undefined -> ets:new(genetica_cognitive_ets,
                             [set, named_table]);
        _ -> nil
    end,
    ets:insert(genetica_cognitive_ets, {simtype, SimType}),
    ok.


genotype_to_phenotype_fn([_, _, _, _, SimType | _]) ->
    init_global_ets(SimType),
    %% Generate fitness fn here?
    fun (G) ->
            Ref = erlang:make_ref(),
            P = #ptype{gtype = G, ref = Ref},
            ets:insert(genetica_cognitive_ets, refit(SimType, P)),
            P
    end.

fitness_fn(_) ->
    fun (#ptype{ref = Ref}, _) ->
            [{Ref, F}] = ets:lookup(genetica_cognitive_ets, Ref),
            F
    end.

crossover_fn([_, _, XoverRate | _]) ->
    fun (G1, G2) ->
            case genetica_utils:random_bit() of
                1 -> B = lists:zip(G1, G2);
                0 -> B = lists:zip(G2, G1)
            end,
            lists:map(fun ({X, Y}) ->
                              case random:uniform() < XoverRate of
                                  true -> X;
                                  false -> Y
                              end
                      end, B)
    end.

mutate_in([P, Divisor], Min, X, Max) ->
    case random:uniform() < P of
        true -> Sigma = (Max - Min)/Divisor,
                genetica_utils:clamp(genetica_utils:rand_gauss(X, Sigma),
                                     Min, Max);
        false -> X
    end.

mutate(M, #vertex{tau=Tau, sigma=Sigma, gain=G} = V) ->
    V#vertex{tau = mutate_in(M, 1, Tau, 2),
             sigma = mutate_in(M, -10, Sigma, 0),
             gain = mutate_in(M, 1, G, 5)};
mutate(M, Weight) ->
    mutate_in(M, -5, Weight, 5).

mutation_fn([P, Divisor | _]) ->
    M = fun (G) ->
                mutate([P, Divisor], G)
        end,
    fun (Geno) ->
            lists:map(M, Geno)
    end.

analyze_fn(Sock, Fitness_fn) ->
    fun (Pop) ->
            Fits = Fitness_fn(Pop),
            {_, #ptype{gtype = Best}} = lists:max(lists:zip(Fits, Pop)),
            Floats = [genetica_utils:avg(Fits), genetica_utils:std_dev(Fits),
                      lists:max(Fits), lists:min(Fits), Best],
            gen_tcp:send(Sock, io_lib:fwrite("~w~n", [Floats])),
            random:seed(now()),
            Refit = genetica_utils:pmap(fun refit/1, Pop),
            ets:select_delete(genetica_cognitive_ets, ?ETS_REF_DELETE),
            %% ^ hack, flip over to gen_server for next task.
            ets:insert(genetica_cognitive_ets, Refit),
            ok
    end.

