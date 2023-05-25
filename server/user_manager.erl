-module(user_manager).
-export([create_account/4, delete_account/4, login/4, logout/4, is_logged_in/3, update_user_score/2, get_user_level/2]).


create_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true ->
            From ! user_exists,
            io:fwrite("User ~p already exists!\n", [User]),
            Registers_updated = Registers;
        false ->
            NewId = maps:size(Registers) + 1,
            Registers_updated = maps:put(NewId, {User, Pwd, 0, 1, false}, Registers),
            From ! success
    end,
    Registers_updated.


delete_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true -> 
            {ok, {Password, _, _}} = maps:find(User, Registers),
            if
                Password == Pwd ->
                    Registers_updated = maps:remove(User, Registers),
                    From ! success;
                true ->
                    io:fwrite("User ~p: Invalid Password!\n", [User]),
                    Registers_updated = Registers,
                    From ! invalid_pwd
            end;
        false ->
            io:fwrite("User ~p does not exist!\n", [User]),
            Registers_updated = Registers,
            From ! invalid_user
    end,
    Registers_updated.


login(Registers, From, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {Password, Wg, _}} -> 
            if
                Password == Pwd ->
                    Registers_updated = maps:update(User, {Pwd, Wg, true}, Registers),
                    From ! success;
                true ->
                    From ! invalid_pwd,
                    io:fwrite("User ~p: Invalid Password!\n", [User]),
                    Registers_updated = Registers
            end;
        _ -> 
            Registers_updated = Registers,
            From ! invalid_user,
            io:fwrite("User ~p does not exist!\n", [User])
    end,
    Registers_updated.


logout(Registers, From, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {Password, Wg, _}} ->
            if
                Password == Pwd ->
                    Registers_updated = maps:update(User, {Pwd, Wg, false}, Registers),
                    From ! success;
                true ->
                    From ! invalid_pwd,
                    io:fwrite("User ~p: Invalid Password!\n", [User]),
                    Registers_updated = Registers
            end;
        _ ->
            Registers_updated = Registers,
            io:fwrite("User ~p does not exist!\n", [User]),
            From ! invalid_user
    end,
    Registers_updated.


is_logged_in(Registers, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {_, Password, _, Login}} ->
            if
                (Password == Pwd) and (Login == true) ->
                    true;
                true ->
                    false
            end;
        _ -> false
    end.

        
update_user_score(Registers, User) ->
    case maps:get(User, Registers) of
        {Pwd, Wg, Login} -> NewRegisters = maps:update(User, {Pwd, Wg + 1, Login}, Registers);
        _ -> NewRegisters = false
    end,
    NewRegisters.


get_user_level(Registers, User) ->
    case maps:get(User, Registers) of
        {_, _, Wg, _} -> Level = (Wg / 2) + 1;
        _ -> Level = false
    end,
    Level.

