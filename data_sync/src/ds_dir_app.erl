%%%-------------------------------------------------------------------------
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%% @copyright 2013 Univerity of Michigan
%%% @doc Something here
%%% @end
%%%-------------------------------------------------------------------------
-module(ds_dir_app).
-export([start/2, stop/1]).
-define(DEFAULT_PORT, 1155).

%%%-------------------------------------------------------------------------
%%% application callbacks
%%%-------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    Port =
            case application:get_env(data_sync, port) of
                {ok, P} -> P;
                undefined -> ?DEFAULT_PORT
            end,
    {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
    case ds_dir_sup:start_link(LSock) of
            {ok, Pid} ->
                ds_dir_sup:start_child(),
                {ok, Pid};
            {Other} ->
                {error, Other}
    end.

stop(_State) ->
    ok.
