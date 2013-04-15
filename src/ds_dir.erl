%%%-------------------------------------------------------------------------
%%% @author V. Glenn Tarcea <gtarcea@umich.edu>
%%% @copyright 2013 Univerity of Michigan
%%% @doc Something here
%%% @end
%%%-------------------------------------------------------------------------

-module(ds_dir).

-behaviour(gen_server).

%% API
-export([start_link/1]).


%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {lsock}).

%%%-------------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------------

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

%%%-------------------------------------------------------------------------
%%% gen_server callbacks
%%%-------------------------------------------------------------------------

init([LSock]) ->
    {ok, #state{lsock = LSock}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, _Socket, RawData}, State) ->
    io:format("From ~p Received: ~p~n", [self(),RawData]),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, _Sock} = gen_tcp:accept(LSock),
    ds_dir_sup:start_child(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




