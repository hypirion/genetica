-module(genetica_utils).
-export([repeatedly/2, random_bit/0, avg/1, std_dev/1, std_dev/2, ffilter/2,
         comp/1, shuffle/1, atom_to_integer/1, atom_to_float/1, atom_append/2,
         pmap/2, clamp/3, rand_between/2, iterate/3, rand_gauss/2, zip/2]).

repeatedly(0, _) ->
    [];
repeatedly(N, Fun) ->
    [Fun() | repeatedly(N - 1, Fun)].

random_bit() ->
    random:uniform(2) - 1.

%% Returns the average of Xs, which is a list of numbers.
avg(Xs) ->
    avg(Xs, 0, 0).

avg([H | T], Len, Sum) ->
    avg(T, Len + 1, Sum + H);
avg([], Len, Sum) ->
    Sum / Len.

%% Returns the standard deviation of Xs, which is a list of numbers.
std_dev(Xs) ->
    std_dev(Xs, avg(Xs)).

%% Returns the standard deviation of Xs, which is a list of numbers whose
%% average is Avg.
std_dev(Xs, Avg) ->
    Sums = lists:foldl(
             fun(V, Acc) -> D = V - Avg, Acc + (D * D) end,
             0, Xs),
    math:sqrt(Sums / (length(Xs) - 1)).

%% Returns the first element in Xs where Pred(X) is true, or nil if none
%% satisfies Pred.
ffilter([H | T], Pred) ->
    case Pred(H) of
        true -> H;
        false -> ffilter(T, Pred)
    end;
ffilter([] , _) -> nil.

%% Returns fun (X) -> F1(F2(...(FN(X)))) end when given [F1, F2, ..., FN]
comp([F]) ->
    F;
comp([F | T]) ->
    G = comp(T),
    fun (X) ->
            F(G(X))
    end.

%% Shuffles a list.
shuffle(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), X} || X <- List])].

atom_to_integer(Atom) ->
    list_to_integer(atom_to_list(Atom)).

atom_to_float(Atom) ->
    list_to_float(atom_to_list(Atom)).

atom_append(Atom, String) ->
    list_to_atom(atom_to_list(Atom) ++ String).

clamp(Actual, Lower, Upper) ->
    max(Lower, min(Actual, Upper)).

rand_between(Lower, Upper) ->
    Diff = Upper - Lower,
    Lower + Diff * random:uniform().

%% Parallel map
pmap(F, L) ->
    S = self(),
    Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
    pmap_harvest(Pids).

pmap_harvest([H|T]) ->
    receive
        {H, Ret} -> [Ret | pmap_harvest(T)]
    end;
pmap_harvest([]) ->
    [].

pmap_f(Parent, F, I) ->
    Parent ! {self(), (catch F(I))}.

iterate(N, F, Init) ->
    iterate(N, F, Init, [Init]).

iterate(0, _F, _Cur, Acc) ->
    lists:reverse(Acc);
iterate(N, F, Cur, Acc) ->
    New = F(Cur),
    iterate(N-1, F, New, [New | Acc]).

%% Box-Muller method
rand_gauss(Mu, Sigma) ->
    U = random:uniform(),
    V = random:uniform(),
    Z = math:sqrt(-2 * math:log(U)) * math:cos(2*math:pi()*V),
    X = Mu + Sigma * Z,
    X.

%% Allows us to zip lists of uneven length
zip(A, B) ->
    zip(A, B, []).

zip([A | RA], [B | RB], Acc) ->
    zip(RA, RB, [{A, B} | Acc]);
zip(_, _, Acc) ->
    lists:reverse(Acc).
