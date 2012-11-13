%%%-------------------------------------------------------------------
%%% @author V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%% @copyright (C) 2012, V. Glenn Tarcea
%%% @doc
%%%
%%% @end
%%% Created : 12 Nov 2012 by V. Glenn Tarcea <glenn.tarcea@gmail.com>
%%%-------------------------------------------------------------------
-module(sf_client).

%% API
-export([send_file/1]).

%% Macros
-define(DEFAULT_PORT, 1055).


%%%===================================================================
%%% API
%%%===================================================================

send_file(Filepath) ->
    %{ok, Fd} = file:open(Filepath, [raw, binary, read]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", ?DEFAULT_PORT, [binary, {packet, raw}, {active, false}]),
    gen_tcp:send(Socket, "/tmp/out.2"),
    {ok, Packet} = gen_tcp:recv(Socket, 2),
    io:format("~s~n", [Packet]),
    file:sendfile(Filepath, Socket),
    gen_tcp:close(Socket),
    ok.
