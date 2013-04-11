-module(mcds_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_PORT, 10101).
-define(DEFAULT_MC_SERVER, "materialscommons.org").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Port = get_env_default(port, ?DEFAULT_PORT),
    Server = get_env_default(mc_server, ?DEFAULT_MC_SERVER),
    {ok, LSocket} = gen_tcp:listen(Port, [{active, true}]),
    {ok, SyncQueue} = get_sync_queue(LSocket),
    mcds_sup:start_link(LSocket, Server, SyncQueue).

stop(_State) ->
    ok.

get_env_default(What, Default) ->
    case application:get_env(ds_dir, What) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

get_sync_queue(LSocket) ->
    ok.