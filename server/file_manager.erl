-module(file_manager).
-export([readContent/1, parser/2, parseSingleField/1, data_to_text/1, account_to_string/1, write_data/2, test/0, file_management/0]).


file_management() ->
    receive
        {write_data, Data, Filename} ->
            write_data(Filename, Data),
            file_management()
    end.


readContent(Filename) -> 
    Result = file:read_file(Filename),
    case Result of
        {ok, Binary} ->
            Info = string:split(Binary, "\n"),
            case Info of
                [<<>>] -> #{};
                _Info -> parser(Info, #{})
            end;
        {error, _} ->
            io:fwrite("Error opening the file\n"),
            #{}
    end.


%UPW -> User, Password, Wg
parser([], Data) -> Data;
parser([H | T], Data) ->
    io:fwrite("Header ~p~n", [H]),
    if 
        H == [<<>>] -> 
            parser(T,Data);
        true ->
            [U | PW] = string:split(H, ","),
            User = parseSingleField(U),
            [P | W] = string:split(PW, ","),
            Pwd = parseSingleField(P),
            %io:fwrite("Pwd ~p~n", [User]),
            Wg = parseSingleField(W),
            %io:fwrite("Wg ~p~n", [Wg]),
            if 
                User == [<<>>] ->
                    Data1 = Data;
                true ->
                    Data1 = maps:put(User, {Pwd, list_to_integer(Wg), false}, Data)
            end,
            if
                T == [] ->
                    Data1;
                true ->
                    parser(string:split(T, "\n"), Data1)
            end
    end.



parseSingleField(Field) ->
    [_ | Value] = string:split(Field, ":"),
    [UserStr] = Value,
    binary_to_list(UserStr).


account_to_string({User, {Pwd, Wg, _}}) ->
    string:join( [string:join(["$USERNAME", User], ":"),
                 string:join(["$PASSWORD" , Pwd],":"),
                 string:join(["$WG" , integer_to_list(Wg)], ":")], ",").


data_to_text([]) -> "";
data_to_text([H | T]) ->
    io:fwrite("Payload: ~p~n", [H]),
    string:join([account_to_string(H), data_to_text(T)], "\n").


write_data(Filename, Accounts) -> 
    file:write_file(Filename, data_to_text(maps:to_list(Accounts))).


test() ->
    Map = readContent("file_syntax.txt"),
    Map1 = maps:put([<<"Goncalo">>], {[<<"1234">>], [<<"5">>], false}, Map),
    write_data("file_syntax.txt", Map1).