-module(game).
-export([game/1]).


game(Pids) ->
    io:format("Pids: ~p~n", [Pids]),
    %?MODULE ! start,
    [From ! start || {From, _, _} <- Pids],
    setup(Pids, #{}).


setup([], Players) ->
    Pid = self(),
    Sender = spawn(fun() ->
                        Data = #{},
                        Data1 = maps:put(players, Players, Data),
                        Pids = maps:keys(Players),
                        Items = spawnItems(4, [], 10),
                        Data2 = maps:put(items, Items, Data1),
                        [Player ! {initMatch, Data2, Pid, match_manager} || Player <- Pids],
                        dataSender(Data, Pid) end),
    keysManager(Players, Sender);
setup([{From, User} | T], Players) ->
    % Keys = #{},
    % Keys = maps:put(w, false, Keys),
    % Keys = maps:put(a, false, Keys),
    % Keys = maps:put(d, false, Keys),
    % Player = spawnPlayer(User, Players, 35, Keys),
    % Players = maps:put(From, Player, Players),
    % setup(T, Players).^
    Keys = maps:put(d, false, maps:put(a, false, maps:put(w, false, #{}))),
    Player = spawnPlayer(User, Players, 35, Keys),
    Players = maps:put(From, Player, Players),
    setup(T, Players).


newPosition(Players, Size) ->
    X = rand:uniform(900),
    Y = rand:uniform(900),
    Valid = validSpawn(X, Y, Size, Players),

    if
        Valid ->
            {X, Y};
        true ->
            newPosition(Players, Size)
    end.


spawnPlayer(User, Players, Size, PressedKeys) ->
    X = rand:uniform(900),
    Y = rand:uniform(900),
    Valid = validSpawn(X, Y, Size, Players),
    if
        Valid ->
            Player = #{},
            Player = maps:put(user, User, Player),
            Player = maps:put(x, X, Player),
            Player = maps:put(y, Y, Player),
            Player = maps:put(pressedKeys, PressedKeys, Player),
            Player = maps:put(score, 0, Player),
            Player = maps:put(speed, 0, Player),
            Player = maps:put(angle, 0, Player),
            Player = maps:put(acceleration, false, Player),
            Player = maps:put(angleVariation, 0.1, Player),
            Player = maps:put(speedVariation, 3.5, Player),
            Player = maps:put(green, false, Player),
            Player = maps:put(blue, false, Player),
            Player;
        true ->
            spawnPlayer(User, Players, Size, PressedKeys)
    end.

spawnItems(0, Items, _) ->
    Items;
spawnItems(Num, Items, Size) ->
    Item = spawnItem(Items, Size),
    spawnItems(Num-1, [Items | Item], Size).


spawnItem(Items, Size) ->
    X = rand:uniform(900),
    Y = rand:uniform(900),
    Num = rand:uniform(3),
    if
        Num == 1 ->
            Type = green;
        Num == 2 ->
            Type = blue;
        true ->
            Type = red
    end,

    Valid = validItemSpawn(X, Y, Size, Items),
    if
        Valid ->
            Item = #{},
            Item = maps:put(type, Type, Item),
            Item = maps:put(x, X, Item),
            Item = maps:put(y, Y, Item),
            Item;
        true ->
            spawnItem(Items, Size)
    end.


validItemSpawn(_, _, _, []) -> true;
validItemSpawn(X, Y, Size, [Item | Items]) ->
    X1 = maps:get(x, Item),
    Y1 = maps:get(y, Item),
    Distance = math:sqrt(math:pow(X1 - X, 2) + math:pow(Y1 - Y, 2)),

    if
        Distance < (Size * 2)+50 ->
            false;
        true ->
            validItemSpawn(X, Y, Size, Items)
    end.


validSpawn(_, _, _, []) -> true;
validSpawn(X, Y, Size, [{_, Player}|Players]) ->
    {ok, X1} = maps:find(x, Player),
    {ok, Y1} = maps:find(y, Player),
    Distance = math:sqrt(math:pow(X1 - X, 2) + math:pow(Y1 - Y, 2)),

    if
        Distance < (Size * 2)+1 ->
            false;
        true ->
            validSpawn(X, Y, Size, Players)
    end.


dataSender(Data, Pid) ->
    receive
        {score, From, Score} ->
            Players = maps:get(players, Data),
            Player = maps:get(From, Players),
            PlayerUpdated = maps:update(score, Score, Player),
            DataUpdated = maps:update(From, PlayerUpdated, Data),
            dataSender(DataUpdated, Pid);
        {quit, From, User} ->
            Players = maps:get(players, Data),
            Pids = maps:keys(Players),
            surrender(From, Pids, Players),
            match_manager ! {leave, From, User},
            success;
        {From, PressedKeys} ->
            Players = maps:get(players, Data),
            Player = maps:get(From, Players),
            PlayerUpdated = maps:update(pressedKeys, PressedKeys, Player),
            PlayersUpdated = maps:update(From, PlayerUpdated, Players),
            DataUpdated = maps:update(players, PlayersUpdated, Data),
            dataSender(DataUpdated, Pid)
        after
            20 -> %VERIFICAR VALOR----------------------------------
                DataUpdated = simulate(Data, Pid),
                dataSender(DataUpdated, Pid)
    end.


simulate(MatchData, Pid) ->
    %atualização dos dados dos jogadores
    Players = maps:get(players, MatchData),
    Pids = maps:keys(Players),
    {UpdatedPlayers, Pids} = keyPressedAction(maps:to_list(Players), #{}, []),
    UpdatedMatchData = maps:update(players, UpdatedPlayers, MatchData),

    %verificação de colisões com items, players e sucessiva atualização de info
    UpdatedMatchData1 = eatingKillingActions(UpdatedMatchData),

    OldPlayers = maps:get(players, MatchData),
    OldItems = maps:get(items, MatchData),
    NewItems = maps:get(items, UpdatedMatchData1),

    DataToSend = #{},
    if
        OldPlayers == UpdatedPlayers ->
            DataToSend1 = DataToSend;
        true ->
            DataToSend1 = maps:put(players, UpdatedPlayers, DataToSend)
    end,
    if
        OldItems == NewItems ->
            DataToSend2 = DataToSend1;
        true ->
            DataToSend2 = maps:put(items, NewItems, DataToSend1)
    end,

    case maps:size(DataToSend2) of
        0 ->
            pass;
        _ ->
            [PlayerPid ! {updateInfo, DataToSend2, Pid} || PlayerPid <- Pids]
    end,

    UpdatedMatchData1.


keysManager(Players, Sender) ->
    receive
        {updatedKey, Key, Status, From} ->
            Player = maps:get(From, Players),
            PressedKeys = maps:get(pressedKeys, Player),
            case Status of
                true ->
                    case Key of
                        "w" ->
                            UpdatedPressedKeys = maps:put(w, true, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "d" ->
                            UpdatedPressedKeys = maps:put(d, true, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "a" ->
                            UpdatedPressedKeys = maps:put(a, true, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "s" ->
                            UpdatedPressedKeys = maps:put(s, true, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender)
                    end;
                false ->
                    case Key of
                        "w" ->
                            UpdatedPressedKeys = maps:put(w, false, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "d" ->
                            UpdatedPressedKeys = maps:put(d, false, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "a" ->
                            UpdatedPressedKeys = maps:put(a, false, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender);
                        "s" ->
                            UpdatedPressedKeys = maps:put(s, false, PressedKeys),
                            Sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated, Sender)
                    end
            end;
        %VERIFICAR SE FALTAM REQUESTS
        {leave, User, Pid} ->
            Sender ! {quit, User, Pid}
    end.


keysActions(Speed, Angle, _, Acceleration, _, []) ->
    {Speed, Angle, Acceleration};
keysActions(Speed, Angle, AngleVariation, Acceleration, SpeedVariation, [{Key, true} | T]) ->
    UpdatedAcc = Acceleration,
    UpdatedSpeed = Speed,
    UpdatedAngle = Angle,

    case Key of
        w ->
            UpdatedAcc = true,
            UpdatedSpeed = SpeedVariation;
        d ->
            UpdatedAngle = Angle + AngleVariation;
        a ->
            UpdatedAngle = Angle - AngleVariation
    end,
    keysActions(UpdatedSpeed, UpdatedAngle, AngleVariation, UpdatedAcc, SpeedVariation, T).


keyPressedAction([], Result, PlayersUpdated) ->
    {Result, PlayersUpdated};
keyPressedAction([{From, Data} | T], Result, PlayersUpdated) ->
    PressedKeys = maps:get(pressedKeys, Data),
    X = maps:get(x, Data),
    Y = maps:get(y, Data),
    Keys = maps:filter(fun(_, V) -> if V == true -> true; true -> false end end, PressedKeys),
    Angle = maps:get(angle, Data),
    Speed = maps:get(speed, Data),
    Acceleration = maps:get(acceleration, Data),
    SpeedVariation = maps:get(speedVariation, Data),
    AngleVariation = maps:get(angleVariation, Data),

    {UpdatedSpeed, UpdatedAngle, UpdatedAcc} = keysActions(Speed, Angle, AngleVariation, Acceleration, SpeedVariation, Keys),

    {UpdatedX, UpdatedY} = updatePosition(X, Y, UpdatedSpeed, UpdatedAngle),

    case {UpdatedX, UpdatedY, UpdatedSpeed, UpdatedAngle, UpdatedAcc} of
        {X, Y, Speed, Angle, Acceleration} ->
            Update = false,
            ResultUpdated = maps:put(From, Data, Result);
        _ ->
            Update = true,
            DataUpdated = maps:update(x, UpdatedX, Data),
            DataUpdated1 = maps:update(y, UpdatedY, DataUpdated),
            DataUpdated2 = maps:update(speed, UpdatedSpeed, DataUpdated1),
            DataUpdated3 = maps:update(angle, UpdatedAngle, DataUpdated2),
            DataUpdated4 = maps:update(acceleration, UpdatedAcc, DataUpdated3),
            ResultUpdated = maps:put(From, DataUpdated4, Result)
    end,

    if
        Update ->
            NewPlayersUpdated = [PlayersUpdated | From];
        true ->
            NewPlayersUpdated = PlayersUpdated
    end,
    keyPressedAction(T, ResultUpdated, NewPlayersUpdated).

respawnPlayers(_, [], UpdatedPlayers) ->
    UpdatedPlayers;
respawnPlayers(Players, [Pid | DeadPids], UpdatedPlayers) ->
    Player = maps:get(Pid, Players),

    {X, Y} = newPosition(Players, 35),
    Keys = #{},
    Keys = maps:put(w, false, Keys),
    Keys = maps:put(a, false, Keys),
    Keys = maps:put(d, false, Keys),

    UpdatedPlayer = maps:put(x, X, Player),
    UpdatedPlayer1 = maps:put(y, Y, UpdatedPlayer),
    UpdatedPlayer2 = maps:put(pressedKeys, Keys, UpdatedPlayer1),
    NewUpdatedPlayers = maps:put(Pid, UpdatedPlayer2, UpdatedPlayers),
    respawnPlayers(Players, DeadPids, NewUpdatedPlayers).



updatePosition(X, Y, Speed, Angle) ->
    UpdatedX = X + (Speed * math:cos(Angle)),
    UpdatedY = Y + (Speed * math:sin(Angle)),
    {UpdatedX, UpdatedY}.


updateScore(Points, Player) ->
    Score = maps:get(score, Player),
    UpdatedPlayer = maps:put(score, (Score + Points), Player),
    UpdatedPlayer.


playerSetItemTrue(Player, Type) ->
    Player1 = maps:put(Type, true, Player),
    case Type of
        green ->
            AngleVariation = maps:get(angleVariation, Player1),
            Player2 = maps:put(angleVariation, AngleVariation*1.5, Player1),
            Player2;
        blue ->
            SpeedVariation = maps:get(speedVariation, Player1),
            Player2 = maps:put(speedVariation, SpeedVariation*1.5, Player1),
            Player2;
        _ ->
            true
    end.


resetVariations(Player) ->
    Player1 = maps:put(speedVariation, 3.5, Player),
    Player2 = maps:put(angleVariation, 0.1, Player1),
    Player3 = maps:put(green, false, Player2),
    Player4 = maps:put(blue, false, Player3),
    Player4.


eatingKillingActions(MatchData) ->
    Players = maps:get(players, MatchData),
    Items = maps:get(items, MatchData),

    {PlayersScoreUpdated, DeadPids} = checkPlayersColisions(maps:to_list(Players), maps:to_list(Players), Players, []),
    UpdatedPlayers = respawnPlayers(PlayersScoreUpdated, DeadPids, #{}),

    {UpdatedPlayers1, NotEatenItems} = checkPlayersItemsColisions(maps:to_list(UpdatedPlayers), Items, UpdatedPlayers, []),
    %ATUALIZAR PLAYERS CONFORME ITEMS CONSUMIDOS

    UpdatedMatchData = maps:put(players, UpdatedPlayers1, MatchData), %ALTERAR PLAYERS

    NumOfItems = length(NotEatenItems),
    if
        NumOfItems < 8 ->
            UpdatedItems = spawnItems(1, NotEatenItems, 10);
        true ->
            UpdatedItems = NotEatenItems
    end,

    UpdatedMatchData1 = maps:put(items, UpdatedItems, UpdatedMatchData),
    UpdatedMatchData1.



checkPlayersColisions([], _, PlayersMap, DeadPids) ->
    {PlayersMap, DeadPids};
checkPlayersColisions([{PidPredator, Predator} | T], PlayersList, PlayersMap, DeadPids) ->
    UpdatedDeadPids = playerColisions({PidPredator, Predator}, PlayersList, DeadPids),
    if
        UpdatedDeadPids == DeadPids ->
            PlayersUpdated = PlayersMap;
        true ->
            PlayerUpdated = updateScore(1, Predator),
            PlayersUpdated = maps:put(PidPredator, PlayerUpdated, PlayersMap)
    end,

    checkPlayersColisions(T, PlayersList, PlayersUpdated, UpdatedDeadPids).


playerColisions(_, [], DeadPids) ->
    DeadPids;
playerColisions({PidPredator, Predator}, [{PidVictim, Victim} | T], DeadPids) ->
    if
        PidPredator == PidVictim ->
            playerColisions({PidPredator, Predator}, T, DeadPids);
        true ->
            Bool = colisionPlayerAction(Predator, Victim),
            if
                Bool ->
                    playerColisions({PidPredator, Predator}, T, [DeadPids | PidVictim]);
                true ->
                    playerColisions({PidPredator, Predator}, T, DeadPids)
            end
    end.


colisionPlayerAction(Predator, Victim) ->
    PredatorDirection = maps:get(angle, Predator),
    VictimDirection = maps:get(angle, Victim),

    Difference = abs(PredatorDirection - VictimDirection),
    if
        Difference < 90 -> %VERIFICAR SE É EM GRAUS OU RADIANOS
            PredatorX = maps:get(x, Predator),
            PredatorY = maps:get(y, Predator),
            VictimX = maps:get(x, Victim),
            VictimY = maps:get(y, Victim),
            Distance = math:sqrt(math:pow(PredatorX - VictimX, 2) + math:pow(PredatorY - VictimY, 2)),

            if
                Distance < (35*2-1) ->
                    true;
                true ->
                    false
            end;
        true ->
            false
    end.


checkPlayersItemsColisions(_, [], PlayersMap, NotEatenItems) ->
    {PlayersMap, NotEatenItems};
checkPlayersItemsColisions([{From, Data} | T], Items, PlayersMap, NotEatenItems) ->
    {UpdatedPlayersMap, UpdatedNotEatenItems} = playerItemsColisions({From, Data}, Items, PlayersMap, NotEatenItems),
    checkPlayersItemsColisions(T, UpdatedNotEatenItems, UpdatedPlayersMap, []).


playerItemsColisions(_, [], PlayersMap, NotEatenItems) ->
    {PlayersMap, NotEatenItems};
playerItemsColisions({From, Data}, [Item | Items], PlayersMap, NotEatenItems) ->
    case colisionPlayerItemAction(Data, Item) of
        green ->
            UpdatedPlayer = playerSetItemTrue(Data, green),
            UpdatedPlayersMap = maps:put(From, UpdatedPlayer, PlayersMap),
            playerItemsColisions({From, Data}, Items, UpdatedPlayersMap, NotEatenItems);
        blue ->
            UpdatedPlayer = playerSetItemTrue(Data, blue),
            UpdatedPlayersMap = maps:put(From, UpdatedPlayer, PlayersMap),
            playerItemsColisions({From, Data}, Items, UpdatedPlayersMap, NotEatenItems);
        red ->
            UpdatedPlayer = resetVariations(Data),
            UpdatedPlayersMap = maps:put(From, UpdatedPlayer, PlayersMap),
            playerItemsColisions({From, Data}, Items, UpdatedPlayersMap, NotEatenItems);
        false ->
            playerItemsColisions({From, Data}, Items, PlayersMap, [NotEatenItems | Item])
    end.


colisionPlayerItemAction(Player, Item) ->
    PlayerX = maps:get(x, Player),
    PlayerY = maps:get(y, Player),
    ItemX = maps:get(x, Item),
    ItemY = maps:get(y, Item),
    Type = maps:get(type, Item),
    Distance = math:sqrt(math:pow(PlayerX - ItemX, 2) + math:pow(PlayerY - ItemY, 2)),

    if
        Distance < (35+10-1) ->
            Type;
        true ->
            false
    end.


surrender(From, [Pid | Pids], Players) ->
    Player = maps:get(Pid, Players),
    User = maps:get(user, Player),

    if
        Pid == From ->
            Pid ! {gameover, Pid, User, 0};
        true ->
            Pid ! {gameover, Pid, User, 1}
    end,
    surrender(From, Pids, Players).
