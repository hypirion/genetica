%%%-------------------------------------------------------------------
%%% File    : genetica_single_sup.erl
%%% Author  : Jean Niklas L'orange <jeannikl@hypirion.com>
%%% Description : 
%%%
%%% Created : 18 Mar 2013 by Jean Niklas L'orange <jeannikl@hypirion.com>
%%%-------------------------------------------------------------------
-module(genetica_single_sup).

-behaviour(supervisor).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start_link/0
        ]).

%%--------------------------------------------------------------------
%% Internal exports
%%--------------------------------------------------------------------
-export([
         init/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Server functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([]) ->
    %% TODO: How to terminate supervisor when we're done? The transport worker
    %% has to figure out when we're done, and terminate its supervisor! (Must be
    %% a cast, as we'll have to respond to the shutdown call the supervisor
    %% sends us)
    TransportWorker = {transport,
                       {genetica_tcp_worker, start_link, []},
                       %% Hurr, need to pass the communication layer.
                       temporary, 2000, worker, [genetica_transport_worker]},
    ComputeWorker = {compute,
                     {genetica_compute_worker, start_link, []},
                     %% Need to connect this to the transport worker...
                     temporary, 2000, worker, [genetica_compute_worker]},
    {ok,{{one_for_all, 0, 1}, %% Terminate everything if something fails
         [TransportWorker, ComputeWorker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
