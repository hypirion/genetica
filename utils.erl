-module(utils).
-export([repeatedly/2]).

repeatedly(0, _) ->
    [];
repeatedly(N, Fun) ->
    [Fun() | repeatedly(N - 1, Fun)].
