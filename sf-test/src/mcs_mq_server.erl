%%%-------------------------------------------------------------------
%%% @author V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%% @copyright (C) 2012, V. Glenn Tarcea
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2012 by V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%%-------------------------------------------------------------------
-module(mcs_mq_server).

-behaviour(gen_qserver).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/2, handle_call/3, handle_cast/2, handle_info/2, stop/1,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {cache_pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(ConnectionSpecs) ->
%    ConnectionSpecs = [
%                       {<<"exchange">>,[{type, <<"topic">>}]}, <<"upload.localhost">>
%                      ],
    gen_qserver:start_link({local, ?SERVER}, ?MODULE, [], [], ConnectionSpecs).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args, CachePid) ->
    {ok, #state{cache_pid = CachePid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({<<Route/binary>>, Payload}, _From, State) ->
    error_logger:info_msg("handle_call - Route: ~p, Payload: ~p~n", [Route, Payload]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({<<Route/binary>>, Payload}, State) ->
    error_logger:info_msg("handle_cast - Route: ~p, Payload: ~p~n", [Route, Payload]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

stop(Pid) ->
    gen_qserver:cast(Pid, stop).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
