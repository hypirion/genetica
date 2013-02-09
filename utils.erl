-module(utils).
-export([repeatedly/2, random_bit/0, avg/1, std_dev/1, std_dev/2]).

repeatedly(0, _) ->
    [];
repeatedly(N, Fun) ->
    [Fun() | repeatedly(N - 1, Fun)].

random_bit() ->
    random:uniform(2) - 1.

avg(Xs) ->
    avg(Xs, 0, 0).

avg([H | T], Len, Sum) ->
    avg(T, Len + 1, Sum + H);
avg([], Len, Sum) ->
    Sum / Len.

std_dev(Xs) ->
    std_dev(Xs, avg(Xs)).

std_dev(Xs, Avg) ->
    Sums = lists:foldl(
             fun(V, Acc) -> D = V - Avg, Acc + (D * D) end,
             0, Xs),
    math:sqrt(Sums / (length(Xs) - 1)).
