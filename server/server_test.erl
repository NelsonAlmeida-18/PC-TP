-module(server_teste).
-export([start/0]).

start() ->
    {ok, ListenSocket} = gen_tcp:listen(9999, [binary, {active, false}]),
    loop(ListenSocket).

loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    loop(ListenSocket).

handle_client(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Request} ->
            % Process the request and generate a response
            Response = "Hello from Erlang",
            gen_tcp:send(Socket, Response);
        {error, Reason} ->
            error_logger:error_msg("Error receiving data from socket: ~p~n", [Reason])
    end,
    gen_tcp:close(Socket).

