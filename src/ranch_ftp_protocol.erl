-module(ranch_ftp_protocol).
-export([start_link/4, init/3]).

start_link(ListenerPid, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport]),
    {ok, Pid}.

init(ListenerPid, Socket, Transport) ->
    io:format("Got connection~n"),
    ok =ranch:accept_ack(ListenerPid),
    Transport:send(Socket, <<"200 MCS File Service Welcome\r\n">>),
    {ok, Data} = Transport:recv(Socket, 0, 30000),
    auth(Socket, Transport, Data).

auth(Socket, Transport, <<"USER ", Rest/bits>>) ->
    io:format("User authenticated ~p~n", [Rest]),
    Transport:send(Socket, <<"230 Auth Ok\r\n">>),
    loop(Socket, Transport).

loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 30000) of
        {ok, Data} ->
            Buffer2 = << Buffer/binary, Data/binary >>,
            {Commands, Rest} = split(Buffer2),
            [handle(Socket, Transport, Commmand) || Command <- Commands],
            loop(Socket, Transport);
        {error, _} ->
            io:format("The client disconnected~n")
    end.

handle(Socket, Transport, Data) ->
    io:format("Command received: ~p~n", [Data]),
    Transport:send(Socket, <<"500 Bad Command\r\n">>).