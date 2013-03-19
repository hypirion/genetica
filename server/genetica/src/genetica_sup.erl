-module(genetica_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Module]) ->
    Listener = {listener,
                {genetica_tcp_listener, start_link, [Port, Module]},
                permanent, 5000, worker, [genetica_tcp_listener]},
    ClientSupervisor = {client_supervisor,
                        {genetica_client_sup, start_link, []},
                        permanent, 5000, supervisor, [genetica_client_sup]},
    RestartStrategy = {one_for_rest, 0, 1}, %% set to 4, 3600 later on.
    {ok, {RestartStrategy, [ClientSupervisor, Listener]}}.
