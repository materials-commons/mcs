-module(mcds_queue_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Queue, Server) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Queue, Server]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([_Queue, _Server]) ->
    {ok, {}}.