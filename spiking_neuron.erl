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

-record(neuron, {gtype, train, spikes}).

parse_args([Fname, Sdm, Timesteps| T ]) ->
    [].

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
    [V || [V, U] <- utils:iterate(?TIMESTEPS, F,
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

genotype_to_phenotype_fn(_) ->
    fun (Genotype) ->
            Train = create_train(Genotype),
            SPos = spike_positions(Train),
            %% TODO: Generate fitness here as well.
            #neuron{gtype=Genotype, train=Train, spikes=SPos}
    end.
