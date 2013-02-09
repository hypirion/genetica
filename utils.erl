-module(utils).
-export([repeatedly/2, random_bit/0]).

repeatedly(0, _) ->
    [];
repeatedly(N, Fun) ->
    [Fun() | repeatedly(N - 1, Fun)].

random_bit() ->
    random:uniform(2) - 1.
