-module(server).
-import(user_manager, [create_account/4, delete_account/4, login/4, logout/4, is_logged_in/3, accounts_management/1]).
-import(file_manager, [readContent/1, write_data/2, file_management/0]).
-export([start/1, server/1, stop/1, lobby/1, acceptor/1, userAuth/1]).


start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.


server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            register(match_manager, spawn(fun() -> lobby([]) end)),
            Filename = "file_syntax.txt",
            Accounts = file_manager:readContent(Filename),
            register(accounts_manager, spawn(fun() -> accounts_management(Accounts) end)),
            register(file_manager, spawn(fun() -> file_management() end)),
            acceptor(LSock);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock) end),
            userAuth(Socket);
        {error, _} ->
            io:fwrite("Error listening to socket")
    end.

request({Request, User, Pwd}) ->
    accounts_manager ! {Request, self(), User, Pwd},
    receive
        {Result, accounts_manager} ->
            Result
    end.

userAuth(Sock) ->
    receive
        %recebe msg do cliente
        {tcp, _, Data} ->
            ListOfInfo = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
            Info = string:split(ListOfInfo, ",", all),
            case Info of
                ["create_account", User, Pwd] ->
                    Result = request({create_account, User, Pwd}),
                    case Result of
                        user_exists ->
                            gen_tcp:send(Sock, "User already exists!\n"),
                            userAuth(Sock);
                            %match_manager ! {newPlayer, User, self()};
                        account_created ->
                            gen_tcp:send(Sock, "User created!\n"),
                            accounts_manager ! write_data,
                            userAuth(Sock)
                    end;
                ["delete_account", User, Pwd] ->
                    Result = request({delete_account, User, Pwd}),
                    case Result of
                        invalid_user ->
                            gen_tcp:send(Sock, "User does not exist!\n");
                        invalid_pwd ->
                            gen_tcp:send(Sock, "Invalid password!\n");
                        account_deleted ->
                            gen_tcp:send(Sock, "Account deleted with success!\n")
                    end,
                    accounts_manager ! write_data,
                    userAuth(Sock);
                ["login", User, Pwd] ->
                    Result = request({login, User, Pwd}),
                    case Result of
                        invalid_user ->
                            gen_tcp:send(Sock, "User does not exist!\n");
                        invalid_pwd ->
                            gen_tcp:send(Sock, "Invalid password!\n");
                        login_sucessfully ->
                            gen_tcp:send(Sock, "Logged in with success!\n")
                    end,
                    userAuth(Sock);
                ["logout", User, Pwd] ->
                    Result = request({logout, User, Pwd}),
                    case Result of
                        invalid_user ->
                            gen_tcp:send(Sock, "User does not exist!\n");
                        logout_sucessfully ->
                            gen_tcp:send(Sock, "Logged out with success!\n")
                    end;
                ["statistics", _] ->
                    accounts_manager ! {statistics, self()},
                    receive
                        Stats ->
                            gen_tcp:send(Sock, Stats)
                    end,
                    userAuth(Sock);
                ["join", User] ->
                    Result = request({is_logged_in, User, something}),
                    case Result of
                        true ->
                            gen_tcp:send(Sock, "User joined the lobby!\n"),
                            accounts_manager ! {user_level, User, self()},
                            receive
                                Level ->
                                    match_manager ! {join, self(), User, Level},
                                    Match = initGame(Sock, User),
                                    userGameFlow(Sock, User, Match)
                            end,
                            gen_tcp:send(Sock, "User joined the lobby!\n");
                        false ->
                            gen_tcp:send(Sock, "User not logged in!\n"),
                            userAuth(Sock)
                    end;
                _ -> io:fwrite("Error!\n")
            end;
        _ -> userAuth(Sock)
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
                            TempPid = [{From, User, UserLevel} | Pids],
                            case matchMaking(TempPid) of
                                {User1, User2} ->
                                    {From1, Username1, _} = User1,
                                    {From2, Username2, _} = User2,
                                    spawn(fun() -> game:game([{From1, Username1}, {From2, Username2}]) end),
                                    NewPids = lists:delete(User1, Pids),
                                    NewPids2 = lists:delete(User2, NewPids),
                                    lobby(NewPids2);
                                false ->
                                    lobby([Pids | {From, User, UserLevel}])
                            end;
                        true ->
                            From ! already_in
                    end
            end,
            From ! success;
        {leave, From, User} -> %MANTER ESTE REQUEST?????? NAO ESTA A SER USADO
            NewPids = lists:delete({From, User}, Pids),
            %io:fwrite("User ~s left the lobby.~n", [User]),
            From ! success,
            lobby(NewPids)
    end.


matchMaking([]) -> false;
matchMaking([H | T]) ->
    Result = sameLevel(T, H),
    case Result of
        {true, User1, User2} -> {User1, User2};
        _ -> matchMaking(T)
    end.


sameLevel(_, []) -> false;
sameLevel([H | T], Elem) ->
    {_, _, Level} = Elem,
    case H of
        {_ , _ , Level2} ->
            if
                Level == Level2 ->
                    {true, Elem, H};
                true ->
                    sameLevel(T, Elem)
            end;
        _ ->
            sameLevel([], Elem)
    end.


initGame(Sock, User) ->
    match_manager ! {join, User, self()},

    receive
        {initMatch, Data, MatchPid, match_manager}->
            gen_tcp:send(Sock, "The game has started\n"),
            sendInitData(Sock, Data),
            MatchPid;
        _ ->
            io:fwrite("User ~s left the match.~n", [User]),
            request({logout, User, something}),
            match_manager ! {leave, User, self()}
    end.


gameOver(Sock, Pid, User, Flag) ->
    if
        Flag == 1 ->
            gen_tcp:send(Sock, "You won!\n"),
            accounts_manager ! {update_victories, User},
            accounts_manager ! write_data;
        Flag == 0 ->
            gen_tcp:send(Sock, "You lost!\n")
    end,

    match_manager ! {leave, Pid, User},
    afterGameOver(Sock, User).


afterGameOver(Sock, User) ->
    receive
        {tcp, _, Data} ->
            Info = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
            Info1 = string:split(Info,",",all),

            case Info1 of
                ["Quit"] ->
                    request({logout, User, something}),
                    userAuth(Sock);
                ["PlayAgain"] ->
                    Result = request({is_logged_in, User, something}),
                    case Result of
                        true ->
                            gen_tcp:send(Sock, "User joined the lobby!\n"),
                            accounts_manager ! {user_level, User, self()},
                            receive
                                Level ->
                                    match_manager ! {join, self(), User, Level},
                                    Match = initGame(Sock, User),
                                    userGameFlow(Sock, User, Match)
                            end;
                        false ->
                            gen_tcp:send(Sock, "User not logged in!\n"),
                            userAuth(Sock)
                    end
            end;
        _ ->
            request({logout, User, something}),
            match_manager ! {leave, User, self()}
    end.


userGameFlow(Sock, User, MatchPid) ->
    receive
        {gameover, Pid, User, Flag} ->
            gameOver(Sock, Pid, User, Flag)
        after 0 ->
            receive
                {gameOver, Pid, User, Flag} ->
                    gameOver(Sock, Pid, User, Flag);
                {updateInfo, UpdatedData, MatchPid} ->
                    sendUpdatedData(Sock, UpdatedData),
                    userGameFlow(Sock, User, MatchPid);
                {tcp, _, Data} ->
                    Info = re:replace(Data,"\\n|\\r", "",[global,{return,list}]),
                    Info1 = string:split(Info,",",all),
                    case Info1 of
                        ["KeyChanged", Key, "True"] ->
                            match_manager ! {keyChanged, Key, true, self()};
                        ["KeyChanged", Key, "False"] ->
                            match_manager ! {keyChanged, Key, false, self()}
                    end,
                    userGameFlow(Sock, User, MatchPid);
                _ ->
                    io:fwrite("User ~s: Error.~n", [User]),
                    request({logout, User, something}),
                    match_manager ! {leave, User, self()}
            end
    end.


sendPlayersData(Sock, Data) ->
    Players = maps:get(players, Data),
    ListOfPlayers = [Player || {_, Player} <- maps:to_list(Players)],
    sendPlayerData(Sock, ListOfPlayers).


sendPlayerData(_, []) ->
    ok;
sendPlayerData(Sock, [H | T]) ->
    User = maps:get(user, H),
    X = maps:get(x, H),
    Y = maps:get(y, H),
    Angle = maps:get(angle, H),
    Score = maps:get(score, H),
    gen_tcp:send(Sock, io_lib:fwrite("P,~s,~w,~w,~w,~w\n", [User, X, Y, Angle, Score])),
    sendPlayerData(Sock, T).


sendItemsData(Sock, Data) ->
    Items = maps:get(items, Data),
    sendItemData(Sock, Items).

sendItemData(_, []) ->
    true;
sendItemData(Sock, [Item | Items]) ->
    X = maps:get(x, Item),
    Y = maps:get(y, Item),
    Type = maps:get(type, Item),
    gen_tcp:send(Sock, io_lib:fwrite("I,~s,~w,~w\n", [Type, X, Y])),
    sendItemData(Sock, Items).


sendInitData(Sock, Data) ->
    sendPlayerData(Sock, Data),
    sendItemsData(Sock, Data).


sendUpdatedData(Sock, Data) ->
    case maps:find(players, Data) of
        {ok, Players} ->
            sendPlayersData(Sock, Players);
        error ->
            pass
    end,

    case maps:find(items, Data) of
        {ok, Items} ->
            sendItemsData(Sock, Items);
        error ->
            pass
    end.
