-module(spiking_neuron).
-import(utils, [atom_to_integer/1, atom_to_float/1, clamp/3, rand_between/2]).
%-export([parse_args/1, random_genotype_fn/1, phenotype_to_genotype_fn/1,
%         genotype_to_phenotype_fn/1, fitness_fn/1, crossover_fn/1,
%         mutation_fn/1, analyze_fn/1]).
-compile(export_all).

-define(SPIKE_THRESHOLD, 35.0).
-define(TAU, 10.0).
-define(EXT_INPUT, 10.0).
-define(TIMESTEPS, 1000).
-define(NEURON_INIT_V, -60).
-define(NEURON_INIT_U, 0).
-define(ACT_K, 5).
-define(ACT_T, 0).
-define(ABCDK_INTERVALS,
        [{0.001, 0.2}, {0.01, 0.3}, {-80, -30}, {0.1, 10}, {0.01, 1.0}]).

-record(neuron, {gtype, train, spikes, fitness=0}).

parse_args([Fname, Sdm | T ]) ->
    [atom_to_list(Fname), Sdm | T].

random_genotype_fn(_) ->
    fun () ->
            A = rand_between(0.001, 0.2),
            B = rand_between(0.01, 0.3),
            C = rand_between(-80, -30),
            D = rand_between(0.1, 10),
            K = rand_between(0.01, 1.0),
            <<A/float, B/float, C/float, D/float, K/float>>
    end.

create_train(<<A/float, B/float, C/float, D/float, K/float>>) ->
    F = fun ([V, U]) ->
                DV = (K*V*V + 5*V + 140 - U + ?EXT_INPUT)/?TAU,
                DU = A * (B*V - U) / ?TAU,
                case V >= ?SPIKE_THRESHOLD of
                    true ->
                        New_V = C + DV,
                        New_U = U + D;
                    false ->
                        New_V = V + DV,
                        New_U = U + DU
                end,
                [min(New_V, ?SPIKE_THRESHOLD), New_U]
        end,
    [V || [V, _] <- utils:iterate(?TIMESTEPS, F,
                                  [?NEURON_INIT_V, ?NEURON_INIT_U])].

spike_positions(Volt_values) ->
    {Firsts, Rest} = lists:split(?ACT_K, Volt_values),
    Middle = (?ACT_K + 1) div 2,
    spike_positions(Firsts, Rest, [], Middle).

spike_positions(H, T, Acc, Pos) ->
    Mid = lists:nth((?ACT_K + 1) div 2, H),
    Max = lists:max(H),
    case Max =:= Mid andalso
         Max >= ?ACT_T of
        true ->  NAcc = [Pos | Acc];
        false -> NAcc = Acc
    end,
    case T of
        [] ->
            lists:reverse(NAcc);
        _  ->
            spike_positions(tl(H) ++ [hd(T)], tl(T), NAcc, Pos + 1)
    end.

genotype_to_phenotype_fn([Fname, Fitfn | _]) ->
    {ok, [Goal]} = file:consult(Fname),
    Goal_spikes = spike_positions(Goal),
    fun (Genotype) ->
            Train = create_train(Genotype),
            SPos = spike_positions(Train),
            Fitness = Fitfn(Goal, Goal_spikes, Train, SPos),
            #neuron{gtype=Genotype, train=Train,
                    spikes=SPos, fitness=Fitness}
    end.

-define(SPIKE_TIME_P, 2.0).

time_sum({TAi, TBi}, Acc) ->
    Delta = math:pow(abs(TAi - TBi), ?SPIKE_TIME_P),
    Acc + Delta.

time_fitness(_G, GSpikes, _A, ASpikes) ->
    Zipped = utils:zip(GSpikes, ASpikes),
    N = length(Zipped),
    Total = lists:foldl(fun time_sum/2, 0, Zipped),
    Tot_sp = Total + spike_penalty(GSpikes, ASpikes, N),
    Res = math:pow(Tot_sp, 1/?SPIKE_TIME_P)/N,
    1 / max(Res, 0.00001).

interval_sum({{TAi_, TBi_}, {TAi, TBi}}, Acc) ->
    Interval_diff = math:pow(abs((TAi - TAi_) - (TBi - TBi_)), ?SPIKE_TIME_P),
    Acc + Interval_diff.

interval_fitness(_G, GSpikes, _A, ASpikes) ->
    Zipped = utils:zip(GSpikes, ASpikes),
    N = length(Zipped),
    Multizipped = utils:zip(Zipped, tl(Zipped)),
    Total = lists:foldl(fun interval_sum/2, 0, Multizipped),
    Tot_sp = Total + spike_penalty(GSpikes, ASpikes, N),
    Res = math:pow(Tot_sp, 1/?SPIKE_TIME_P)/(N - 1),
    1 / max(Res, 0.00001).

spike_penalty(A, B, N) ->
    abs(length(A) - length(B))
        * (?TIMESTEPS + 1) / max(2*N, 1).

waveform_fitness(G, _GSpikes, A, _ASpikes) ->
    Sum = lists:sum([math:pow(abs(Ai - Gi), ?SPIKE_TIME_P)
                     || {Gi, Ai} <- lists:zip(G, A)]),
    Res = math:pow(Sum, 1/?SPIKE_TIME_P)/(?TIMESTEPS + 1),
    1 / max(Res, 0.00001).

fitness_fn(_) ->
    fun (Ptype, _Others) ->
            Ptype#neuron.fitness
    end.

crossover_avg(G1, G2) ->
    crossover_avg(G1, G2, <<>>).

crossover_avg(<<>>, <<>>, Res) ->
    Res;
crossover_avg(<<A/float, TA/binary>>, <<B/float, TB/binary>>, <<Res/binary>>) ->
    C = (A + B) / 2,
    crossover_avg(TA, TB, <<Res/binary, C/float>>).

crossover_sel(G1, G2) ->
    crossover_sel(G1, G2, <<>>).

crossover_sel(<<A/float, TA/binary>>, <<B/float, TB/binary>>, <<Res/binary>>) ->
    case utils:random_bit() of
        0 -> C = A;
        1 -> C = B
    end,
    crossover_sel(TA, TB, <<Res/binary, C/float>>).

crossover_fn(_) ->
    fun crossover_avg/2.

mutation(P, <<X/float, R/binary>>, [{L, U, Stddev} | T]) ->
    Rmut = mutation(P, R, T),
    case random:uniform() =< P of
        true -> Unclamped = utils:rand_gauss(X, Stddev),
                Clamped = utils:clamp(Unclamped, L, U),
                <<Clamped/float, Rmut/binary>>;
        false -> <<X/float, Rmut/binary>>
    end;
mutation(_, <<>>, []) -> <<>>.

mutation_fn([_, Mutprob, Mutrate | _]) ->
    Range_and_stddev = [{L, U, (U-L)/6} || {L, U} <- ?ABCDK_INTERVALS],
    fun (Gtype) ->
            case random:uniform() =< Mutprob of
                true -> mutation(Mutrate, Gtype, Range_and_stddev);
                false -> Gtype
            end
    end.
