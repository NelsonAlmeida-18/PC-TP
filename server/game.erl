-module(game).
-export([]).


game(Pids) ->
    ?MODULE ! start,
    [From ! start || {From, Id, UserLevel} <- Pids],
    Players = setup(Pids, []),
    


setup([], PositionsTaken) -> #{};
setup([{From, Id} | Pids], PositionsTaken) ->
    case random:uniform(2) of
        1 -> Color = blue;
        2 -> Color = red
    end,
    %verificar dimensoes do mapa
    X = random:uniform(900),
    Y = random:uniform(900),
    Position = {X, Y},
    case lists:member(Position, PositionsTaken) of
        false ->
            Map = setup(Pids, [Position | PositionsTaken]),
            maps:put(Id, {From, Position, Color}, Map);
        true ->
            setup([{From, Id} | Pids], PositionsTaken)
    end.
