-module(file_manager).
-export([readContent/1, parser/2, parseSingleField/1, data_to_text/1, account_to_string/1, write_data/2, test/0]).


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
    [ID | UPW] = string:split(H, ","),
    IDnum = parseSingleField(ID),
    [U | PW] = string:split(UPW, ","),
    User = parseSingleField(U),
    [P | W] = string:split(PW, ","),
    Pwd = parseSingleField(P),
    Wg = parseSingleField(W),
    NewUser = maps:put(IDnum, {User, Pwd, Wg, false}, Data),
    if
        T == [] ->
            NewUser;
        true ->
            parser(string:split(T, "\n"), NewUser)
    end.



parseSingleField(Field) ->
    [_ | Value] = string:split(Field, ":"),
    Value.


account_to_string({IDnum, {User, Pwd, Wg, _}}) ->
    string:join(["$ID:" ++ IDnum, 
                 "$USERNAME:" ++ User,
                 "$PASSWORD:" ++ Pwd,
                 "$WG:" ++ Wg], ",").


data_to_text([]) -> "";
data_to_text([H | T]) ->
    string:join([account_to_string(H), data_to_text(T)], "\n").


write_data(Filename, Accounts) -> 
    file:write_file(Filename, data_to_text(maps:to_list(Accounts))).


test() ->
    Map = readContent("file_syntax.txt"),
    User = maps:get([<<"id">>], Map),
    Map1 = maps:put([<<"2">>], {[<<"goncalo">>], [<<"1234">>], [<<"5">>], false}, #{}),
    write_data("file_syntax.txt", Map1).