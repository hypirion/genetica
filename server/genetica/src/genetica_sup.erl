-module(genetica_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Server = {genetica_server, {genetica_server, start_link, []},
              permanent, 2000, worker, [genetica_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.
