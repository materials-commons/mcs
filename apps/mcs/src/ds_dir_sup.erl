%%%-------------------------------------------------------------------------
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%% @copyright 2013 Univerity of Michigan
%%% @doc Something here
%%% @end
%%%-------------------------------------------------------------------------
-module(ds_dir_sup).
-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------------
start_link(LSock) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

start_child() ->
    supervisor:start_child(?SERVER, []).

%%%-------------------------------------------------------------------------
%%% supervisor callbacks
%%%-------------------------------------------------------------------------

init([LSock]) ->
    Server = {ds_dir, {ds_dir, start_link, [LSock]},
                temporary, brutal_kill, worker, [ds_dir]},
    Children = [Server],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.