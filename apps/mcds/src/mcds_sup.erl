
-module(mcds_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, [LSock, Queue]}, temporary, brutal_kill, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port, Server) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Server]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, Server]) ->
    MCDS_Console_Sup = {mcds_console_sup, {mcds_console_sup, start_link, [Port]},
                permanent, 2000, supervisor, [mcds_console_sup]},
    MCDS_Queue_Sup = {mcd_queue_sup, {mcds_queue_sup, start_link, [Server]},
                        permanent, 2000, supervisor, [mcds_queue_sup]},
    Children = [MCDS_Console_Sup, MCDS_Queue_Sup],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, { RestartStrategy, Children}}.

