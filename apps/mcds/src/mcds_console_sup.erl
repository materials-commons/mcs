-module(mcds_console_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([_Port]) ->
    {ok, {}}.