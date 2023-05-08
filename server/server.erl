-module(server).
-import(users_manager, [create_account/4, delete_account/4, login/4, logout/4, is_logged_in/3]).
-import(file_manager, [readContent/1, write_data/2]).
-export([start/1, server/1, stop/1, lobby/2, acceptor/1, userAuth/1, loop/2]).


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
                ["delete_account", Id, Pwd] ->
                    ?MODULE ! {delete_account, Id, Pwd, self()},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        invalid_pwd -> gen_tcp:send(Sock, "invalid_pwd\n");
                        invalid_user -> gen_tcp:send(Sock, "invalid_user\n")
                    end,
                    userAuth(Sock);
                ["login", Id, Pwd] ->
                    ?MODULE ! {login, self(), Id, Pwd},
                    receive
                        success -> gen_tcp:send(Sock, "success\n");
                        invalid_pwd -> gen_tcp:send(Sock, "invalid_pwd\n");
                        invalid_user -> gen_tcp:send(Sock, "invalid_user\n")
                    end,
                    userAuth(Sock);
                ["logout", Id, Pwd] ->
                    ?MODULE ! {logout, self(), Id, Pwd},
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
                            List = [string:join([Id, User, integer_to_list(Wg)], " - ") || {Id, User, Wg} <- Stats],
                            StatsList = string:join(List, "\n"),
                            gen_tcp:send(Sock, string:join([StatsList, "\n"], ""))
                    end,
                    userAuth(Sock);
                ["join", Id, UserLevel] ->
                    ?MODULE ! {join, self(), Id, UserLevel},
                    receive
                        success -> gen_tcp:send("success\n");
                        not_logged_in -> gen_tcp:send("not_logged_in\n")
                    end,
                    userAuth(Sock);
                _ -> io:fwrite("Error!\n")
            end
    end.


loop(Registers, Lobby) ->
    receive
        {start, From} ->
            NewLobby = spawn(fun() -> lobby([], 0) end),
            loop(Registers, NewLobby);
        {finish, From, Id} ->
            io:fwrite("User ~p Won!\nGame over!\n", [Id]),
            From ! winner,
            NewRegisters = user_manager:update_user_score(Registers, Id),
            if
                NewRegisters == false ->
                    loop(Registers, Lobby);
                true ->
                    loop(NewRegisters, Lobby)
            end;
        {create_account, User, Pwd, From} ->
            NewRegisters = user_manager:create_account(Registers, From, User, Pwd),
            loop(NewRegisters, Lobby);
        {delete_account, Id, Pwd, From} ->
            NewRegisters = user_manager:delete_account(Registers, From, Id, Pwd),
            loop(NewRegisters, Lobby);
        {login, From, Id, Pwd} ->
            NewRegisters = user_manager:login(Registers, From, Id, Pwd),
            loop(NewRegisters, Lobby);
        {logout, From, Id, Pwd} ->
            NewRegisters = user_manager:logout(Registers, From, Id, Pwd),
            loop(NewRegisters, Lobby);
        {statistics, From} ->
            Stats = [{Id, User, Wg} || {Id, {User, _, Wg, _}} <- Registers],
            From ! Stats,
            loop(Registers, Lobby);
        {join, Id, Pwd, From} ->
            Status = user_manager:is_logged_in(Registers, Id, Pwd),
            
            if
                Status -> 
                    UserLevel = user_manager:get_user_level(Registers, Id),
                    Lobby ! {join, Id, From, UserLevel};
                true ->
                    From ! not_logged_in
            end,
            loop(Registers, Lobby)
    end.


%Pids é uma lista com o PID e identificador de cada user
%representa o lobby
lobby(Pids, Level) ->
    receive
        {join, From, Id, UserLevel} ->
            if
                Pids == [] ->
                    NewPids = Pids ++ [{From, Id}],
                    lobby(NewPids, UserLevel);
                true ->
                    if
                        UserLevel == Level ->
                            NewPids = Pids ++ [{From, Id}],
                            Size = length(NewPids),
                            case Size of
                                2 -> game:game(NewPids);%começa o jogo
                                _ -> lobby(NewPids, Level)
                            end;
                        true ->
                            lobby(Pids, Level)
                    end
            end,
            From ! success;
        {leave, From, Id} ->
            NewPids = lists:delete({From, Id}, Pids),
            From ! success,
            NewPids
    end.

        
            
