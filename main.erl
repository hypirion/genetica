-module(main).
-export([start/1]).
-import(conversions, [bitstr_to_list/1, list_to_bitstr/1]).
-import(utils, [atom_to_integer/1, atom_to_float/1, atom_append/2]).
-import(selection_mechanisms, [roulette_selection_fn/2, sigma_scale/2,
                               boltzmann_scale/2, rank_scale/2]).

start([AGenerations, APopcount, ASel_method, AK, AP,
       AEval_method, AProtocol, AM, Module | T]) ->
    %% Make run truly random
    random:seed(now()),
    %% Argument parsing from here on
    [Generations, Popcount, K, M] =
        [atom_to_integer(X) || X <- [AGenerations, APopcount, AK, AM]],
    P = atom_to_float(AP),
    [Sel_metfn, Eval_method, Protocol] =
        [atom_append(X, Y) ||
            {X, Y} <- lists:zip([ASel_method, AEval_method, AProtocol],
                                ["_selection_fn", "_scale", "_fn"])],
    {rand, R, p_to_g, PG, g_to_p, GP,
     fitness, F, cross, C, mut, Mut} = fetch_fns(Module, Module:parse_args(T)),
    %% Parsing done
    Fitness = add_fitness_fn(F, fun selection:Eval_method/2),
    Sel_method = selection:Sel_metfn([K, P]),
    Make_child = make_child_fn(Sel_method, C, Mut, PG, GP),
    Devel_and_select = selection:Protocol(Make_child, Sel_method, 
                                          Fitness, [Popcount, M]),
    Initpop = generate_random_pop(Popcount, R, GP),
    Analyzefn = analyze_fn(only_fitness_fn(F)),
    Analyzefn(Initpop),
    genetica_loop(Generations - 1, Initpop, Analyzefn, Devel_and_select).

generate_random_pop(Popcount, Rand_gtype, GtoP) ->
    [GtoP(Genome) || Genome <- utils:repeatedly(Popcount, Rand_gtype)].

genetica_loop(0, _Pop, _Analyzefn, _Develop_and_select) ->
    done;
genetica_loop(Iters, Pop, Analyzefn, Develop_and_select) ->
    Newpop = Develop_and_select(Pop, [Iters]),
    Analyzefn(Newpop),
    genetica_loop(Iters - 1, Newpop, Analyzefn, Develop_and_select).

analyze_fn(Fitness_fn) ->
    fun (Pop) ->
            Fits = Fitness_fn(Pop),
            Floats = [utils:avg(Fits), utils:std_dev(Fits), lists:max(Fits),
                      lists:min(Fits)],
            io:format("~w ~w ~w ~w~n", Floats)
    end.

add_fitness_fn(F, Scale) ->
    fun (Pop, Scale_args) ->
            Unscaled = [{indiv, I, fitness, F(I, Pop)} || I <- Pop],
            Scale(Unscaled, Scale_args)
    end.

only_fitness_fn(F) ->
    fun (Pop) ->
            [F(I, Pop) || I <- Pop]
    end.

make_child_fn(Sel_method, Crossfn, Mutfn, PG, GP) ->
    Parentfn = pick_parents_fn(Sel_method),
    Cproduce = child_producer_fn(Crossfn, Mutfn),
    fun (FPop) ->
            [PG1, PG2] = [PG(X) || X <- Parentfn(FPop)],
            CG = Cproduce(PG1, PG2),
            GP(CG)
    end.

pick_parents_fn(Sel_method) ->
    fun (FPop) ->
            FP1 = Sel_method(FPop),
            {indiv, P1, fitness, _} = FP1,
            {indiv, P2, fitness, _} = Sel_method(FPop -- [FP1]),
            [P1, P2]
    end.

child_producer_fn(Crossfn, Mutfn) ->
    fun (PG1, PG2) ->
            {CG1, CG2} = Crossfn(PG1, PG2),
            case utils:random_bit() of
                0 -> Mutfn(CG1);
                1 -> Mutfn(CG2)
            end
    end.

fetch_fns(Module, Opts) ->
    Rand_gtype = Module:random_genotype_fn(Opts),
    PtoG = Module:phenotype_to_genotype_fn(Opts),
    GtoP = Module:genotype_to_phenotype_fn(Opts),
    Fitness = Module:fitness_fn(Opts),
    Crossfn = Module:crossover_fn(Opts),
    Mutfn = Module:mutation_fn(Opts),
    {rand, Rand_gtype, p_to_g, PtoG, g_to_p, GtoP,
     fitness, Fitness, cross, Crossfn, mut, Mutfn}.

%% (1) choose a genetic representation
%% (2) build a population
%% (3) design a fitness function
%% (4) choose a selection operator
%% (5) choose a recombination operator
%% (6) choose a mutation operator
%% (7) devise a data analysis procedure.
