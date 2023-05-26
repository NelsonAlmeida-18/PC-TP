-module(server).
-import(users_manager, [create_account/4, delete_account/4, login/4, logout/4, is_logged_in/3]).
-import(file_manager, [readContent/1, write_data/2]).
-export([start/1, server/1, stop/1, lobby/1, acceptor/1, userAuth/1, loop/2]).


start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.


server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    case Result of
        {ok, LSock} ->
            Lobby = spawn(fun() -> lobby([], 0) end),
            spawn(fun() -> acceptor(LSock) end),
            Filename = "file_syntax.txt",
            loop(file_manager:readContent(Filename), Lobby);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


acceptor(LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock) end),
    userAuth(Socket).


userAuth(Sock) ->
    receive
        %recebe msg do cliente
        {tcp, _, Data} ->
            ListOfInfo = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
            Info = string:split(ListOfInfo, ",", all),
            case Info of
                ["create_account", User, Pwd] ->
                    ?MODULE ! {create_account, User, Pwd, self()},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        user_exists -> gen_tcp:send(Sock, "user_exists\n")
                    end,
                    userAuth(Sock);
                ["delete_account", User, Pwd] ->
                    ?MODULE ! {delete_account, User, Pwd, self()},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        invalid_pwd -> gen_tcp:send(Sock, "invalid_pwd\n");
                        invalid_user -> gen_tcp:send(Sock, "invalid_user\n")
                    end,
                    userAuth(Sock);
                ["login", User, Pwd] ->
                    ?MODULE ! {login, self(), User, Pwd},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        invalid_pwd -> gen_tcp:send(Sock, "invalid_pwd\n");
                        invalid_user -> gen_tcp:send(Sock, "invalid_user\n")
                    end,
                    userAuth(Sock);
                ["logout", User, Pwd] ->
                    ?MODULE ! {logout, self(), User, Pwd},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        invalid_pwd -> gen_tcp:send(Sock, "invalid_pwd\n");
                        invalid_user -> gen_tcp:send(Sock, "invalid_user\n")
                    end,
                    userAuth(Sock);
                ["statistics", _] ->
                    ?MODULE ! {statistics, self()},
                    receive
                        Stats ->
                            List = [string:join([User, integer_to_list(Wg)], " - ") || {User, Wg} <- Stats],
                            StatsList = string:join(List, "\n"),
                            gen_tcp:send(Sock, string:join([StatsList, "\n"], ""))
                    end,
                    userAuth(Sock);
                ["join", User, UserLevel] ->
                    ?MODULE ! {join, self(), User, UserLevel},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        not_logged_in -> gen_tcp:send(Sock, "not_logged_in\n")
                    end,
                    userAuth(Sock);
                _ -> io:fwrite("Error!\n")
            end;
        _ -> userAuth(Sock)
    end.


loop(Registers, Lobby) ->
    receive
        {start, From} ->
            NewLobby = spawn(fun() -> lobby([], 0) end),
            loop(Registers, NewLobby);
        {finish, From, User} ->
            io:fwrite("User ~p Won!\nGame over!\n", [User]),
            From ! winner,
            NewRegisters = user_manager:update_user_score(Registers, User),
            if
                NewRegisters == false ->
                    loop(Registers, Lobby);
                true ->
                    loop(NewRegisters, Lobby)
            end;
        {create_account, User, Pwd, From} ->
            NewRegisters = user_manager:create_account(Registers, From, User, Pwd),
            loop(NewRegisters, Lobby);
        {delete_account, User, Pwd, From} ->
            NewRegisters = user_manager:delete_account(Registers, From, User, Pwd),
            loop(NewRegisters, Lobby);
        {login, From, User, Pwd} ->
            NewRegisters = user_manager:login(Registers, From, User, Pwd),
            loop(NewRegisters, Lobby);
        {logout, From, User, Pwd} ->
            NewRegisters = user_manager:logout(Registers, From, User, Pwd),
            loop(NewRegisters, Lobby);
        {statistics, From} ->
            Stats = [{User, Wg} || {User, {_, Wg, _}} <- Registers],
            From ! Stats,
            loop(Registers, Lobby);
        {join, User, Pwd, From} ->
            Status = user_manager:is_logged_in(Registers, User, Pwd),

            if
                Status ->
                    UserLevel = user_manager:get_user_level(Registers, User),
                    Lobby ! {join, User, From, UserLevel};
                true ->
                    From ! not_logged_in
            end,
            loop(Registers, Lobby)
    end.


%Pids é uma lista com o PID e identificador de cada user
%representa o lobby
lobby(Pids) ->
    receive
        {join, From, User, UserLevel} ->
            if
                Pids == [] ->
                    NewPids = [{From, User, UserLevel}],
                    lobby(NewPids);
                true ->
                    case lists:member({From, User, UserLevel}, Pids) of
                        false ->
                            case matchMaking([Pids | {From, User, UserLevel}]) ->
                                {User1, User2} ->
                                    {From1, Username1, _} = User1,
                                    {From2, Username2, _} = User2,
                                    game:game([{From1, Username1}, {From2, Username2}]),
                                    NewPids = lists:delete(User1, Pids),
                                    %ELIMINAR O USER2??????
                                    lobby(NewPids);
                                false ->
                                    lobby([Pids | {From, User, UserLevel}])
                            end;
                        true ->
                            From ! already_in
                    end,
            end,
            From ! success;
        {leave, From, User} ->
            NewPids = lists:delete({From, User}, Pids),
            From ! success,
            NewPids
    end.


matchMaking([]) -> false;
matchMaking([H | T]) ->
    case sameLevel(H, T) of
        {true, User1, User2} -> {User1, User2};
        false -> matchMaking(T)
    end.


sameLevel(Elem, []) -> false;
sameLevel(Elem, [H | T]) ->
    {From, User, Level} = Elem,
    {From2, User2, Level2} = H,
    if
        Level == Level2 ->
            {true, Elem, H};
        true ->
            sameLevel(Elem, T)
    end.
