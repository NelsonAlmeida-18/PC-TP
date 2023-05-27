-module(user_manager).
-export([update_user_score/2, get_user_level/2, accounts_management/1, get_statistics/2, user_logged_in/3]).


accounts_management(Accounts) ->
    receive
        {create_account, From, User, Pwd} ->
            NewAccounts = create_account(Accounts, From, User, Pwd),
            accounts_management(NewAccounts);
        {delete_account, From, User, Pwd} ->
            NewAccounts = delete_account(Accounts, From, User, Pwd),
            accounts_management(NewAccounts);
        {login, From, User, Pwd} ->
            NewAccounts = login(Accounts, From, User, Pwd),
            accounts_management(NewAccounts);
        {logout, From, User, _} ->
            NewAccounts = logout(Accounts, From, User),
            accounts_management(NewAccounts);
        {update_victories, User} -> 
            NewAccounts = update_user_score(Accounts, User),
            accounts_management(NewAccounts);
        {logged_in_users, From} ->
            logged_in_accounts(Accounts, From),
            accounts_management(Accounts);
        {is_logged_in, From, User, _} ->
            user_logged_in(Accounts, User, From),
            accounts_management(Accounts);
        {user_level, User, From} ->
            From ! get_user_level(Accounts, User),
            accounts_management(Accounts);
        {statistics, From} ->
            get_statistics(Accounts, From),
            accounts_management(Accounts);
        {write_data} ->
            file_manager ! {write_data, Accounts, "file_syntax.txt"};
        {stop, From} ->
            From ! {stopped, accounts_manager}
    end.
        



create_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true ->
            From ! {user_exists, accounts_manager},
            Registers_updated = Registers;
        false ->
            NewId = maps:size(Registers) + 1,
            Registers_updated = maps:put(NewId, {User, Pwd, 0, 1, false}, Registers),
            From ! {account_created, accounts_manager}
    end,
    Registers_updated.


delete_account(Registers, From, User, Pwd) -> 
    case maps:is_key(Registers, User) of
        true -> 
            {ok, {Password, _, _}} = maps:find(User, Registers),
            if
                Password == Pwd ->
                    Registers_updated = maps:remove(User, Registers),
                    From ! {account_deleted, accounts_manager};
                true ->
                    io:fwrite("User ~p: Invalid Password!\n", [User]),
                    Registers_updated = Registers,
                    From ! {invalid_pwd, accounts_manager}
            end;
        false ->
            io:fwrite("User ~p does not exist!\n", [User]),
            Registers_updated = Registers,
            From ! {invalid_user, accounts_manager}
    end,
    Registers_updated.


login(Registers, From, User, Pwd) ->
    case maps:find(Registers, User) of
        {ok, {Password, Wg, _}} -> 
            if
                Password == Pwd ->
                    Registers_updated = maps:update(User, {Pwd, Wg, true}, Registers),
                    From ! {login_sucessfully, accounts_manager};
                true ->
                    io:fwrite("User ~p: Invalid Password!\n", [User]),
                    Registers_updated = Registers,
                    From ! {invalid_pwd, accounts_manager}
            end;
        _ -> 
            Registers_updated = Registers,
            From ! {invalid_user, accounts_manager},
            io:fwrite("User ~p does not exist!\n", [User])
    end,
    Registers_updated.


logout(Registers, From, User) ->
    case maps:find(Registers, User) of
        {ok, {Pwd, Wg, _}} ->
            Registers_updated = maps:update(User, {Pwd, Wg, false}, Registers),
            From ! {logout_successfully, accounts_manager};
        _ ->
            Registers_updated = Registers,
            io:fwrite("User ~p does not exist!\n", [User]),
            From ! {invalid_user, accounts_manager}
    end,
    Registers_updated.


user_logged_in(Accounts, User, From) ->
    Account = maps:get(User, Accounts),
    {_, _, Status}  = Account,
    
    case Status of
        true ->
            From ! true;
        false ->
            From ! false
    end.


logged_in_accounts(Accounts, From) ->
    List = [User || {User, {_, _, true}} <- maps:to_list(Accounts)],
    From ! {{logged_in, List}, accounts_manager}.


get_statistics(Accounts, From) ->
    AccountsList = maps:to_list(Accounts),
    List = [string:join([User, integer_to_list(Wg)], " - ") || {User, _, Wg, _} <- AccountsList],
    Stats = string:join(List, "\n"),
    From ! Stats.
    
        
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
