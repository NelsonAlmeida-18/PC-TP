-module(users_manager).
-export([create_account/4, delete_account/4, login/4, logout/4]).


create_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true ->
            From ! user_exists,
            Registers_updated = Registers;
        false ->
            Registers_updated = maps:put(User, {Pwd, 0, false}, Registers),
            From ! success;
    end,
    Registers_updated.


delete_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true -> 
            {ok, {Password, Level, Login}} = maps:find(User, Registers),
            if
                Password == Pwd ->
                    Registers_updated = maps:remove(User, Registers),
                    From ! success;
                true ->
                    Registers_updated = Registers,
                    From ! invalid_pwd;
            end;
        false ->
            Registers_updated = Registers,
            From ! invalid_user;
    end,
    Registers_updated.


login(Registers, From, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {Password, Level, Login}} -> 
            if
                Password == Pwd ->
                    Registers_updated = maps:update(User, {Pwd, Level, true}, Registers),
                    From ! success;
                true ->
                    From ! invalid_pwd,
                    Registers_updated = Registers;
            end;
        _ -> 
            Registers_updated = Registers,
            From ! invalid_user;
    end,
    Registers_updated.


logout(Registers, From, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {Password, Level, Login}} ->
            if
                Password == Pwd ->
                    Registers_updated = maps:update(User, {Pwd, Level, false}, Registers),
                    From ! success;
                true ->
                    From ! invalid_pwd,
                    Registers_updated = Registers;
            end;
        _ ->
            Registers_updated = Registers,
            From ! invalid_user;
    end,
    Registers_updated.

        
                
