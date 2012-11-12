-module(sf).
-export([send_file/0]).

send_file() ->
    {ok, Descriptor} = file:open("", [raw, binary, read]),
    spawn_link(?MODULE, sendfile_server, [self()]),
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 99514, [binary, {packet,0}, {active, false}]),
    send_file_data(Socket, Descriptor),
    file:close(Descriptor),
    gen_tcp:close(Socket),
    ok.

send_file_data(Socket, Descriptor) ->
    ok.

sendfile_server(ClientPid) ->
    {ok, LSocket} = gen_tcp:listen(99514, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    sendfile_do_recv(Socket, true),
    ok.

sendfile_do_recv(Socket, true) ->
    {ok, OutDescr} = file:open("/tmp/out", [raw, binary, write]),
    sendfile_do_recv(Socket, OutDescr),
    ok;
sendfile_do_recv(Socket, OutDescr) ->

