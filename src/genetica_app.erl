-module(genetica_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    application:start(genetica_app).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    genetica_sup:start_link().

stop(_State) ->
    ok.
