-module(utils).
-export([repeatedly/2, random_bit/0, avg/1, std_dev/1, std_dev/2, ffilter/2,
         comp/1, shuffle/1, atom_to_integer/1, atom_to_float/1]).

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
