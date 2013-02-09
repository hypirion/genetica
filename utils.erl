-module(utils).
-export([repeatedly/2, random_bit/0, avg/1, std_dev/1, std_dev/2, ffilter/2]).

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
