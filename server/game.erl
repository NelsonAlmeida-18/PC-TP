-module(game).
-export([game/2]).


game(P1, P2) ->
    %?MODULE ! start,
    %[From ! start || {From, _, _} <- Pids],
    setup(P1,P2, #{}).

setup({Pid1, User1}, {Pid2, User2}, Players) ->
    MatchPid = self(),
    Keys1 = #{},
    Keys1W = maps:put(w, false, Keys1),
    Keys1A = maps:put(a, false, Keys1W),
    Keys1D = maps:put(d, false, Keys1A),
    Player1 = spawnPlayer(User1, Players, 35, Keys1D),
    Keys2 = #{},
    Keys2W = maps:put(w, false, Keys2),
    Keys2A = maps:put(a, false, Keys2W),
    Keys2D = maps:put(d, false, Keys2A),
    Players1 = maps:put(Pid1, Player1, Players),
    Player2 = spawnPlayer(User2, Players1, 35, Keys2D),
    Players2 = maps:put(Pid2, Player2, Players1),
    

    register(sender,spawn(fun() -> 
        Data = #{},
        Data1 = maps:put(players, Players2, Data),
        Pids = maps:keys(Players2),
        Items = spawnItems(4, [], 10),
        Data2 = maps:put(items, Items, Data1),
        Data3 = maps:put(time, 120000, Data2),
        StartTime = os:timestamp(),
        [Player ! {initMatch, Data2, MatchPid, match_manager} || Player <- Pids],
        dataSender(Data3, MatchPid, StartTime) end)),
    
    register(keysManager, spawn(fun() -> keysManager(Players2) end)),
    register(bonusManager, spawn(fun() -> bonusManager([]) end)).


newPosition(Players, Size) ->
    X = rand:uniform(900),
    Y = rand:uniform(900),
    Valid = validSpawn(X, Y, Size, maps:to_list(Players)),

    if
        Valid ->
            {X, Y};
        true ->
            newPosition(Players, Size)
    end.
    

spawnPlayer(User, Players, Size, PressedKeys) ->
    X = rand:uniform(900),
    Y = rand:uniform(900),
    Valid = validSpawn(X, Y, Size, maps:to_list(Players)),
    if
        Valid ->
            Player = #{},
            Player1 = maps:put(user, User, Player),
            Player2 = maps:put(x, X, Player1),
            Player3 = maps:put(y, Y, Player2),
            Player4 = maps:put(pressedKeys, PressedKeys, Player3),
            Player5 = maps:put(score, 0, Player4),
            Player6 = maps:put(speed, 0, Player5),
            Player7 = maps:put(angle, 0, Player6),
            Player8 = maps:put(acceleration, false, Player7),
            Player9 = maps:put(angleVariation, 0.1, Player8),
            Player10 = maps:put(speedVariation, 3.5, Player9),
            Player10;
        true ->
            spawnPlayer(User, Players, Size, PressedKeys)
    end.

spawnItems(0, Items, _) ->
    Items;
spawnItems(Num, Items, Size) ->
    Item = spawnItem(Items, Size),
    spawnItems(Num-1, [Item | Items], Size).


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
            Item1 = maps:put(type, Type, Item),
            Item2 = maps:put(x, X, Item1),
            Item3 = maps:put(y, Y, Item2),
            Item3;
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


getLoser([], {LoserPid, _}) ->
    LoserPid;
getLoser([{From, Player} | Players], {LoserPid, LowestScore}) ->
    Score = maps:get(score, Player),
    if
        Score > LowestScore ->
            getLoser(Players, {LoserPid, LowestScore});
        Score < LowestScore ->
            getLoser(Players, {From, Score});
        true ->
            Flag = 2,
            Flag
    end.


dataSender(Data, Pid, StartTime) ->
    receive
        {score, From, Score} ->
            Players = maps:get(players, Data),
            Player = maps:get(From, Players),
            PlayerUpdated = maps:update(score, Score, Player),
            PlayersUpdated = maps:update(From, PlayerUpdated, Players),
            DataUpdated = maps:update(players, PlayersUpdated, Data),
            dataSender(DataUpdated, Pid, StartTime);
        {quit, From, User} ->
            Players = maps:get(players, Data),
            Pids = maps:keys(Players),
            surrender(From, Pids, Players),
            match_manager ! {leave, From, User},
            success;
        {From, PressedKeys} ->
            Players = maps:get(players, Data),
            Player = maps:get(From, Players),
            PlayerUpdated = maps:put(pressedKeys, PressedKeys, Player),
            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
            DataUpdated = maps:put(players, PlayersUpdated, Data),
            dataSender(DataUpdated, Pid, StartTime);
        {bonusOver, From, Type} ->
            Players = maps:get(players, Data),
            Player = maps:get(From, Players),
            case Type of
                green ->
                    AngleVariation = maps:get(angleVariation, Player),
                    UpdatedPlayer = maps:put(angleVariation, AngleVariation/1.5, Player);
                blue ->
                    SpeedVariation = maps:get(speedVariation, Player),
                    UpdatedPlayer = maps:put(speedVariation, SpeedVariation/1.5, Player);
                _ ->
                    UpdatedPlayer = Player
            end,
            PlayersUpdated = maps:put(From, UpdatedPlayer, Players),
            DataUpdated = maps:put(players, PlayersUpdated, Data),
            dataSender(DataUpdated, Pid, StartTime);
        resetVariations ->
            Players = maps:get(players, Data),
            Pids = maps:keys(Players),
            UpdatedPlayers = resetVariations(Players, Pids),
            DataUpdated = maps:put(players, UpdatedPlayers, Data),
            dataSender(DataUpdated, Pid, StartTime);
        timesUp ->
            Players = maps:get(players, Data),
            Pids = maps:keys(Players),
            Result = getLoser(maps:to_list(Players), {0, 1000000}),
            surrender(Result, Pids, Players)
        after
            20 -> %VERIFICAR VALOR----------------------------------
                DataUpdated = simulate(Data, Pid, StartTime),
                dataSender(DataUpdated, Pid, StartTime)
    end.


timestamp_to_secs({_, Secs, MicroSecs}) ->
    (Secs * 1000000 + MicroSecs) / 1000000.


simulate(MatchData, Pid, StartTime) ->
    %atualização dos dados dos jogadores
    Players = maps:get(players, MatchData),
    Pids = maps:keys(Players),

    {UpdatedPlayers, _} = keyPressedAction(maps:to_list(Players), #{}, []),

    UpdatedMatchData = maps:update(players, UpdatedPlayers, MatchData),
    %verificação de colisões com items, players e sucessiva atualização de info
    UpdatedMatchData1 = eatingKillingActions(UpdatedMatchData),

    OldTime = maps:get(time, UpdatedMatchData1),
    CurrentTime = timestamp_to_secs(os:timestamp()),
    NewTime = CurrentTime - timestamp_to_secs(StartTime),
    RemainingTime = OldTime - NewTime,

    UpdatedMatchData2 = maps:put(time, RemainingTime, UpdatedMatchData1),
    
    OldPlayers = maps:get(players, MatchData),
    OldItems = maps:get(items, MatchData),
    NewItems = maps:get(items, UpdatedMatchData2),

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
    DataToSend3 = maps:put(time, RemainingTime, DataToSend2),

    case maps:size(DataToSend3) of
        0 ->
            pass;
        _ ->
            [PlayerPid ! {updateInfo, DataToSend3, Pid} || PlayerPid <- Pids]
    end,

    case RemainingTime > 0 of
        false ->
            sender ! timesUp;
        _ ->
            pass
    end,

    UpdatedMatchData2.


keysManager(Players) ->
    receive
        {keyChanged, Key, Status, From} ->
            Player = maps:get(From, Players),
            PressedKeys = maps:get(pressedKeys, Player),
            case Status of
                "true" ->
                    case Key of
                        "w" ->
                            UpdatedPressedKeys = maps:put(w, true, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "d" ->
                            UpdatedPressedKeys = maps:put(d, true, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "a" ->
                            UpdatedPressedKeys = maps:put(a, true, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        _ -> 
                            ok
                    end;
                "false" ->
                    case Key of
                        "w" ->
                            UpdatedPressedKeys = maps:put(w, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "d" ->
                            UpdatedPressedKeys = maps:put(d, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        "a" ->
                            UpdatedPressedKeys = maps:put(a, false, PressedKeys),
                            sender ! {From, UpdatedPressedKeys},
                            PlayerUpdated = maps:put(pressedKeys, UpdatedPressedKeys, Player),
                            PlayersUpdated = maps:put(From, PlayerUpdated, Players),
                            keysManager(PlayersUpdated);
                        _ -> 
                            ok
                    end
            end;
        %VERIFICAR SE FALTAM REQUESTS
        {leave, User, Pid} ->
            sender ! {quit, User, Pid}
    end.


keysActions(Speed, Angle, _, Acceleration, _, []) ->
    {Speed, Angle, Acceleration};
keysActions(Speed, Angle, AngleVariation, Acceleration, SpeedVariation, [{Key, Status} | T]) ->
    case {Key, Status} of
        {w, true} ->
            UpdatedAcc = true,
            UpdatedSpeed = SpeedVariation,
            UpdatedAngle = Angle;
        {w, false} ->
            UpdatedAcc = false,
            UpdatedSpeed = 0,
            UpdatedAngle = Angle;
        {d, true} -> 
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            Temp = Angle + AngleVariation,
            Pi = math:pi() * 2,
            if
                Temp > Pi ->
                    UpdatedAngle = Temp - Pi;
                true ->
                    UpdatedAngle = Temp
            end;
        {d, false} ->
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedAngle = Angle;
        {a, true} -> 
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            Temp = Angle - AngleVariation,
            if
                Temp < 0 ->
                    UpdatedAngle = (math:pi() * 2) + Temp;
                true ->
                    UpdatedAngle = Temp
            end;
        {a, false} -> 
            UpdatedAcc = Acceleration,
            UpdatedSpeed = Speed,
            UpdatedAngle = Angle
    end,
    keysActions(UpdatedSpeed, UpdatedAngle, AngleVariation, UpdatedAcc, SpeedVariation, T).


keyPressedAction([], Result, PlayersUpdated) ->
    {Result, PlayersUpdated};
keyPressedAction([{From, Data} | T], Result, PlayersUpdated) ->
    PressedKeys = maps:get(pressedKeys, Data),
    X = maps:get(x, Data),
    Y = maps:get(y, Data),
    Angle = maps:get(angle, Data),
    Speed = maps:get(speed, Data),
    Acceleration = maps:get(acceleration, Data),
    SpeedVariation = maps:get(speedVariation, Data),
    AngleVariation = maps:get(angleVariation, Data),

    {UpdatedSpeed, UpdatedAngle, UpdatedAcc} = keysActions(Speed, Angle, AngleVariation, Acceleration, SpeedVariation, maps:to_list(PressedKeys)),

    {UpdatedX, UpdatedY} = updatePosition(X, Y, UpdatedSpeed, UpdatedAngle),

    case {UpdatedX, UpdatedY, UpdatedSpeed, UpdatedAngle, UpdatedAcc} of
        {X, Y, Speed, Angle, Acceleration} ->
            Update = false,
            ResultUpdated = maps:put(From, Data, Result);
        _ ->
            Update = true,
            DataUpdated = maps:put(x, UpdatedX, Data),
            DataUpdated1 = maps:put(y, UpdatedY, DataUpdated),
            DataUpdated2 = maps:put(speed, UpdatedSpeed, DataUpdated1),
            DataUpdated3 = maps:put(angle, UpdatedAngle, DataUpdated2),
            DataUpdated4 = maps:put(acceleration, UpdatedAcc, DataUpdated3),
            ResultUpdated = maps:put(From, DataUpdated4, Result)
    end,

    if
        Update ->
            NewPlayersUpdated = [From | PlayersUpdated];
        true ->
            NewPlayersUpdated = PlayersUpdated
    end,
    keyPressedAction(T, ResultUpdated, NewPlayersUpdated).


respawnPlayers(_, _, UpdatedPlayers, []) ->
    UpdatedPlayers;
respawnPlayers(Players, DeadPids, UpdatedPlayers, [Pid|PlayersPids]) ->
    case lists:member(Pid, DeadPids) of
        true -> 
            Player = maps:get(Pid, Players),
            {X, Y} = newPosition(Players, 35),
            Keys = #{},
            Keys1 = maps:put(w, false, Keys),
            Keys2 = maps:put(a, false, Keys1),
            Keys3 = maps:put(d, false, Keys2),
            
            UpdatedPlayer = maps:put(x, X, Player),
            UpdatedPlayer1 = maps:put(y, Y, UpdatedPlayer),
            UpdatedPlayer2 = maps:put(pressedKeys, Keys3, UpdatedPlayer1),
            NewUpdatedPlayers = maps:put(Pid, UpdatedPlayer2, UpdatedPlayers),
            respawnPlayers(Players, DeadPids, NewUpdatedPlayers, PlayersPids);

        false ->
            Player = maps:get(Pid, Players),
            NewUpdatedPlayers = maps:put(Pid, Player, UpdatedPlayers),
            respawnPlayers(Players, DeadPids, NewUpdatedPlayers, PlayersPids)
    end.


updatePosition(X, Y, Speed, Angle) ->
    UpdatedX = X + (Speed * math:cos(Angle)),
    UpdatedY = Y + (Speed * math:sin(Angle)),
    {UpdatedX, UpdatedY}.


resetVariations(Players, []) ->
    Players;
resetVariations(Players, [Pid | Pids]) ->
    Player = maps:get(Pid, Players),
    Player1 = maps:put(speedVariation, 3.5, Player),
    Player2 = maps:put(angleVariation, 0.1, Player1),
    UpdatedPlayers = maps:put(Pid, Player2, Players),
    resetVariations(UpdatedPlayers, Pids).


bonusTimer(Type, From) ->
    receive
        timeout ->
            ok
    after
        5000 ->
            sender ! {bonusOver, From, Type},
            bonusManager ! {timeout, From, self()}
    end.
            

%Pids -> Pid do player associado ao processo 
bonusManager(Pids) ->
    receive
        resetAllBonus ->
            [TimerPid ! timeout || {_, TimerPid} <- Pids],
            sender ! resetVariations,
            bonusManager([]);
        {Type, From} ->
            Pid = spawn(fun() -> bonusTimer(Type, From) end),
            bonusManager([{From, Pid} | Pids]);
        {timeout, From, TimerPid} ->
            UpdatedPids = lists:delete({From, TimerPid}, Pids),
            bonusManager(UpdatedPids)
    end.


eatingKillingActions(MatchData) ->
    Players = maps:get(players, MatchData),
    Items = maps:get(items, MatchData),
    Keys = maps:keys(Players),

    {PlayersScoreUpdated, DeadPids} = checkPlayersColisions(maps:to_list(Players), maps:to_list(Players), Players, Keys),
    %Keys = maps:keys(PlayersScoreUpdated),
    UpdatedPlayers = respawnPlayers(PlayersScoreUpdated, DeadPids, #{}, Keys),
    %io:fwrite("UP: ~p~n", [UpdatedPlayers]),
    {UpdatedPlayers1, NotEatenItems} = checkPlayersItemsColisions(maps:to_list(UpdatedPlayers), Items, UpdatedPlayers, Items),
    %io:fwrite("UP1: ~p~n", [UpdatedPlayers1]),

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
            Score = maps:get(score, Predator),
            PlayerUpdated = maps:put(score, Score+1, Predator),
            PlayersUpdated = maps:put(PidPredator, PlayerUpdated, PlayersMap);
        true -> 
            PlayersUpdated = maps:put(PidPredator, Predator, PlayersMap)
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
                    playerColisions({PidPredator, Predator}, [], DeadPids);
                true ->
                    UpdatedDeadPids = lists:delete(PidVictim, DeadPids),
                    playerColisions({PidPredator, Predator}, T, UpdatedDeadPids)
            end
    end.


colisionPlayerAction(Predator, Victim) ->
    PiMid = math:pi()/2,
    PredatorDirection = maps:get(angle, Predator),
    %VictimDirection = maps:get(angle, Victim),
    PredatorX = maps:get(x, Predator),
    PredatorY = maps:get(y, Predator),
    VictimX = maps:get(x, Victim),
    VictimY = maps:get(y, Victim),

    %DirectionVector = {PredatorX - VictimX, PredatorY - VictimY},
    %NormalizedVector = normalizeVector(DirectionVector),
    Distance = math:sqrt(math:pow(PredatorX - VictimX, 2) + math:pow(PredatorY - VictimY, 2)),
    Angle = math:atan2(VictimY - PredatorY, VictimX - PredatorX),
    AngleDiff = normalizeAngle(Angle - PredatorDirection),


    %HigherRange = VictimDirection + PiMid,
    %LowerRange = VictimDirection - PiMid,

    if
        (Distance < (35*2-1)) and (AngleDiff < PiMid) ->
            true;
        true ->
            false
    end.


checkPlayersItemsColisions([], _, PlayersMap, NotEatenItems) ->
    {PlayersMap, NotEatenItems};
checkPlayersItemsColisions(_, [], PlayersMap, NotEatenItems) ->
    {PlayersMap, NotEatenItems};
checkPlayersItemsColisions([{From, Data} | T], Items, PlayersMap, NotEatenItems) ->
    {UpdatedPlayersMap, UpdatedNotEatenItems} = playerItemsColisions({From, Data}, Items, PlayersMap, NotEatenItems),
    checkPlayersItemsColisions(T, UpdatedNotEatenItems, UpdatedPlayersMap, UpdatedNotEatenItems).


playerItemsColisions(_, [], PlayersMap, NotEatenItems) ->
    {PlayersMap, NotEatenItems};
playerItemsColisions({From, Data}, [Item | Items], PlayersMap, NotEatenItems) ->
    case colisionPlayerItemAction(Data, Item) of
        green ->
            AngleVariation = maps:get(angleVariation, Data),
            UpdatedPlayer = maps:put(angleVariation, AngleVariation*1.5, Data),
            UpdatedPlayersMap = maps:put(From, UpdatedPlayer, PlayersMap),
            UpdatedNotItem = lists:delete(Item,NotEatenItems),
            bonusManager ! {green, From},
            playerItemsColisions({From, Data}, [], UpdatedPlayersMap, UpdatedNotItem);
        blue ->
            SpeedVariation = maps:get(speedVariation, Data),
            UpdatedPlayer = maps:put(speedVariation, SpeedVariation*1.5, Data),
            UpdatedPlayersMap = maps:put(From, UpdatedPlayer, PlayersMap),
            UpdatedNotItem = lists:delete(Item,NotEatenItems),
            bonusManager ! {blue, From},
            playerItemsColisions({From, Data}, [], UpdatedPlayersMap, UpdatedNotItem);
        red ->
            UpdatedPlayersMap = PlayersMap,
            UpdatedNotItem = lists:delete(Item,NotEatenItems),
            bonusManager ! resetAllBonus,
            playerItemsColisions({From, Data}, [], UpdatedPlayersMap, UpdatedNotItem);
        false ->
            UpdatedPlayersMap = maps:put(From, Data, PlayersMap),
            playerItemsColisions({From, Data}, Items, UpdatedPlayersMap, NotEatenItems)
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


surrender(_, [], _) ->
    ok;
surrender(From, [Pid | Pids], Players) ->
    Player = maps:get(Pid, Players),
    User = maps:get(user, Player),

    if 
        From == 2 ->
            Pid ! {gameover, Pid, User, 2};
        Pid == From ->
            Pid ! {gameover, Pid, User, 0};
        true ->
            Pid ! {gameover, Pid, User, 1}
    end,
    surrender(From, Pids, Players).



normalizeAngle(Angle) ->
    NormalizedAngle = math:fmod(Angle + 2 * math:pi(), 2 * math:pi()),
    NormalizedAngle.