-module(tcp_server).
-export([start/0]).

start() ->
    {ok, Listen} = gen_tcp:listen(1234, [binary, {packet, 0}, {reuseaddr, true}]),
    accept_loop(Listen).

accept_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(Listen).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Received: ~s~n", [Data]), % Print the received message
            gen_tcp:send(Socket, Data),
            gen_tcp:flush(Socket), % Flush the output
            handle_client(Socket);
        {error, _} ->
            ok
    end.
